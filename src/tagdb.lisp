;;;; Tagdb --- Tag-based command-line database tool


;;; Copyright (C) 2014-2019 Teemu Likonen <tlikonen@iki.fi>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; The license text: <http://www.gnu.org/licenses/gpl-3.0.html>


(defpackage #:tagdb
  (:use #:cl #:common #:database)
  (:import-from #:split-sequence #:split-sequence)
  (:export #:main #:start #:command-help))

(in-package #:tagdb)


(defclass record ()
  ((id :reader id :initarg :id)
   (created :reader created :initarg :created)
   (modified :reader modified :initarg :modified)
   (tags :reader tags :initarg :tags)
   (content :reader content :initarg :content)))


(defclass output-format ()
  ((records :accessor records :initarg :records :initform nil)
   (verbose :reader verbose :initarg :verbose :initform nil)
   (quiet :reader quiet :initarg :quiet :initform nil)
   (short :reader short :initarg :short :initform nil)))


(defclass text (output-format) nil)
(defclass text-color (text) nil)
(defclass text-editor (text) nil)
(defclass org-mode (output-format) nil)


(defun get-default-format ()
  (query-1 "SELECT value FROM maintenance WHERE key = 'output format'"))


(defun set-default-format (format)
  (query "UPDATE maintenance SET value = ~A WHERE key = 'output format'"
         (sql-string-esc format))
  format)


(defun assert-db-write-access ()
  (handler-case (change-counter-add 0)
    (sqlite:sqlite-error ()
      (tagdb-error "Couldn't access the database. It's probably locked."))))


(defun valid-tag-name-p (tag-name)
  (and (plusp (length tag-name))
       (every (lambda (char)
                (and (not (eql #\space char))
                     (graphic-char-p char)))
              tag-name)))


(defun delete-unused-tags ()
  (query "DELETE FROM tags WHERE id IN ~
        (SELECT tags.id FROM tags LEFT JOIN record_tag AS j ~
        ON tags.id = j.tag_id WHERE j.tag_id IS NULL)"))


(defun db-insert-record (text)
  (let ((now (get-universal-time)))
    (query "INSERT INTO records (created, modified, content) ~
                VALUES (~D, ~D, ~A)"
           now now (sql-string-esc text))
    (prog1 (query-last-insert-rowid)
      (change-counter-add 1))))


(defun db-modify-record (record-id text)
  (query "UPDATE records SET modified = ~D, content = ~A WHERE id = ~D"
         (get-universal-time) (sql-string-esc text) record-id)
  (change-counter-add 1)
  record-id)


(defun db-delete-record (record-id)
  (query "DELETE FROM records WHERE id = ~D" record-id)
  (change-counter-add 1)
  record-id)


(defun db-insert-tag (tag-name)
  (query "INSERT INTO tags (name) VALUES (~A)" (sql-string-esc tag-name))
  (prog1 (query-last-insert-rowid)
    (change-counter-add 1)))


(defun insert-or-get-tag (tag-name)
  (let ((id (query-1 "SELECT id FROM tags WHERE name = ~A"
                     (sql-string-esc tag-name))))
    (or id (db-insert-tag tag-name))))


(defun db-modify-record-tag-connection (record-id new-tag-ids)
  (setf new-tag-ids (remove-duplicates new-tag-ids))
  (let ((old-tag-ids (query-nconc "SELECT tag_id FROM record_tag ~
                                        WHERE record_id = ~D"
                                  record-id)))

    (loop :with old := (set-difference old-tag-ids new-tag-ids)
          :for tag-id :in old
          :do (query "DELETE FROM record_tag ~
                        WHERE record_id = ~D AND tag_id = ~D"
                     record-id tag-id)
          :finally
          (when old
            (change-counter-add (length old))))

    (loop :with new := (set-difference new-tag-ids old-tag-ids)
          :for tag-id :in new
          :do (query "INSERT INTO record_tag (record_id, tag_id) ~
                        VALUES (~D, ~D)" record-id tag-id)
          :finally
          (when new
            (change-counter-add (length new))))

    (values new-tag-ids old-tag-ids)))


(defun assert-tag-names (tag-names)
  (unless tag-names
    (tagdb-error "No tags. At least one tag is required."))
  (loop :for tag-name :in (etypecase tag-names
                            (list tag-names)
                            (string (list tag-names)))
        :unless (valid-tag-name-p tag-name)
          :do (tagdb-error "\"~A\" is not a valid tag name." tag-name)
        :finally (return t)))


(defun new-record (text tag-names)
  (let ((record-id (db-insert-record text))
        (tag-ids (loop :for tag-name :in tag-names
                       :collect (insert-or-get-tag tag-name))))
    (db-modify-record-tag-connection record-id tag-ids)
    (values record-id tag-ids)))


(defun modify-record (record-id text tag-names)
  (let ((tag-ids (loop :for tag-name :in tag-names
                       :collect (insert-or-get-tag tag-name))))
    (db-modify-record record-id text)
    (db-modify-record-tag-connection record-id tag-ids)
    (values record-id tag-ids)))


(defun delete-record (record-id)
  (let ((tag-ids (query-1 "SELECT count(*) FROM record_tag ~
                        WHERE record_id = ~D" record-id)))
    (db-delete-record record-id)
    (change-counter-add tag-ids)
    record-id))


(defun find-records (tag-names)
  (let ((record-ids (loop :for tag :in tag-names
                          :collect
                          (query-nconc "SELECT j.record_id ~
                                FROM record_tag AS j ~
                                LEFT JOIN tags AS t ON j.tag_id = t.id ~
                                WHERE t.name LIKE ~A"
                                       (sql-like-esc tag
                                                     :wild-start t
                                                     :wild-end t))
                          :into collection
                          :finally
                          (return (delete-duplicates
                                   (reduce #'nintersection collection)))))
        (tags nil))

    (unless record-ids
      (error 'records-not-found))

    (loop :for record-id :in record-ids
          :for record-tag-names := (query-nconc "SELECT t.name ~
                        FROM record_tag AS j ~
                        LEFT JOIN tags AS t ON j.tag_id = t.id ~
                        WHERE j.record_id = ~D" record-id)
          :collect (cons record-id (sort record-tag-names #'string-lessp))
          :into collection
          :finally (unless (setf tags collection)
                     (error 'records-not-found)))

    (loop :for (id . names) :in tags
          :for (created modified content) := (first (query "~
                                SELECT created, modified, content ~
                                FROM records WHERE id = ~D" id))
          :collect (make-instance 'record :id id
                                  :created created
                                  :modified modified
                                  :tags names :content content)
          :into collection
          :finally (return (sort collection #'string-lessp
                                 :key (lambda (item)
                                        (format nil "~{~A ~}" (tags item))))))))


(defun hash-record-id (id)
  (let ((*print-base* 36))
    (princ-to-string id)))


(defun format-time (universal-time)
  (local-time:format-timestring
   nil (local-time:universal-to-timestamp universal-time)
   :format '((:year 4) "-" (:month 2) "-" (:day 2) " "
             (:hour 2) ":" (:min 2) ":" (:sec 2) :gmt-offset)))


(defgeneric print-records (format &optional stream))


(defmethod print-records ((format text) &optional (stream *standard-output*))
  (labels ((term-color (&optional true)
             (if (typep format 'text-color)
                 (format nil "~C[~Am" #\Esc (if true "0;32" "0"))
                 ""))

           (record-loop (function)
             (loop :for (record . rest) :on (records format)
                   :for n :upfrom 1
                   :do (funcall function record n)
                   :if rest :do (terpri stream)))

           (taglist (prefix record)
             (format stream "~&~A~A" (term-color t) prefix)
             (loop :with column-max := 78
                   :with column-min := (length prefix)
                   :with column := column-min
                   :for (tag . rest) :on (tags record)
                   :do
                   (format stream " ~A" tag)
                   (incf column (1+ (length tag)))
                   (when (and rest (> (1+ (length (first rest)))
                                      (- column-max column)))
                     (setf column column-min)
                     (format stream "~&~A~A" (term-color t) prefix))))

           (format-contents (string)
             (if (and (short format) (not (typep format 'text-editor)))
                 (subseq string 0 (position #\Newline string))
                 string)))

    (let ((fn (cond
                ((typep format 'text-editor)
                 (lambda (record n)
                   (taglist (format nil "# Id: ~A Tags:" (hash-record-id n))
                            record)
                   (format stream "~%~%~A~&" (format-contents
                                              (content record)))))

                ((verbose format)
                 (lambda (record n)
                   (declare (ignore n))
                   (format stream "~&~A# Created:  ~A~%"
                           (term-color t) (format-time (created record)))
                   (format stream "~&~A# Modified: ~A~%"
                           (term-color t) (format-time (modified record)))
                   (taglist "# Tags:" record)
                   (format stream "~A~%~%~A~&" (term-color nil)
                           (format-contents (content record)))))

                ((quiet format)
                 (lambda (record n)
                   (declare (ignore n))
                   (format stream "~&~A~&" (format-contents
                                            (content record)))))

                (t (lambda (record n)
                     (declare (ignore n))
                     (taglist "# Tags:" record)
                     (format stream "~A~%~%~A~&" (term-color nil)
                             (format-contents (content record))))))))

      (record-loop fn))))


(defmethod print-records ((format org-mode) &optional
                                              (stream *standard-output*))
  (flet ((taglist (prefix record)
           (format stream "~&~A" prefix)
           (loop :with column-max := 78
                 :with column-min := (length prefix)
                 :with column := column-min
                 :for (tag . rest) :on (tags record)
                 :do
                 (format stream " ~A" tag)
                 (incf column (1+ (length tag)))
                 (when (and rest (> (1+ (length (first rest)))
                                    (- column-max column)))
                   (setf column column-min)
                   (format stream "~&~A" prefix))))

         (org-header-line-p (string)
           (loop :with stars := 0
                 :for c :across string
                 :do (cond ((eql c #\space)
                            (return (plusp stars)))
                           ((eql c #\*)
                            (incf stars))
                           (t (return nil))))))

    (loop :for (record . rest) :on (records format)
          :do (with-input-from-string (s (content record))
                (loop :initially
                      (let ((topic (read-line s nil)))
                        (cond ((not topic))
                              ((string= "* " (subseq topic
                                                     0 (min 2 (length topic))))
                               (format stream "~A~%" topic))
                              (t (format stream "* ~A~%" topic)))
                        (taglist "# Tags:" record)
                        (terpri stream))

                      :for line := (read-line s nil)
                      :while line
                      :do (if (org-header-line-p line)
                              (format stream "*~A~%" line)
                              (format stream "~A~%" line))))
          :if rest :do (terpri stream))))


(defun db-find-tags (&optional tag-name)
  (query "SELECT count(t.id) AS count, t.name FROM tags AS t ~
        JOIN record_tag AS j ON t.id = j.tag_id ~
        WHERE t.name LIKE ~A GROUP BY t.name"
         (sql-like-esc (or tag-name "")
                       :wild-start t
                       :wild-end t)))


(defun print-tags (&optional tag-name)
  (let ((tags (db-find-tags tag-name)))
    (if tags
        (loop :with width := (length (princ-to-string
                                      (reduce #'max tags :key #'first)))
              :for (count tag) :in (sort tags #'string-lessp :key #'second)
              :do (message "~V<~D~> ~A~%" width count tag))
        (tagdb-error "No tags found."))))


(defmacro with-temp-file (file &body body)
  (let ((name (gensym "FILENAME")))
    `(handler-case
         (let* ((,name (pathconv:pathname
                        (nth-value 1 (sb-posix:mkstemp "/tmp/tagdb-XXXXXX"))))
                (,file ,name))
           (unwind-protect (progn ,@body)
             (delete-file ,name)))
       (sb-posix:syscall-error ()
         (tagdb-error "Couldn't create a temporary file.")))))


(defun run-text-editor (pathname)
  (let ((editor (split-sequence #\space (sb-posix:getenv "EDITOR")
                                :remove-empty-subseqs t)))

    (check-type pathname (or pathname string))

    (unless (every #'stringp editor)
      (tagdb-error "Please set EDITOR variable."))

    (sb-ext:run-program
     (first editor)
     (append (rest editor)
             (list (pathconv:namestring pathname)))
     :search t :wait t :pty nil :input t :output t :error *error-output*)))


(defun empty-string-p (string)
  (every (lambda (char)
           (or (eql #\space char)
               (not (graphic-char-p char))))
         string))


(defun create-new-record-from-stream (tag-names stream)
  (loop :with text := (make-array 10 :adjustable t :fill-pointer 0)
        :with start
        :with end
        :for line-number :upfrom 0
        :for line := (read-line stream nil)
        :while line

        :do
        (vector-push-extend line text)
        (cond ((and (not start)
                    (not (empty-string-p line)))
               (setf start line-number
                     end line-number))
              ((not (empty-string-p line))
               (setf end line-number)))

        :finally
        (if start
            (new-record (with-output-to-string (out)
                          (loop :for i :from start :upto end
                                :do (write-line (aref text i) out)))
                        tag-names)
            (tagdb-error "Empty file. Aborting."))))


(defun create-and-edit-new-record (tag-names)
  (with-temp-file tempname
    (run-text-editor tempname)
    (with-open-file (file tempname :direction :input)
      (create-new-record-from-stream tag-names file))))


(defun parse-record-header (line)
  (let ((words (split-sequence #\space line :remove-empty-subseqs nil)))
    (when (and (string= "#" (nth 0 words))
               (string= "Id:" (nth 1 words))
               (every #'alphanumericp (nth 2 words))
               (string= "Tags:" (nth 3 words)))
      (values (nth 2 words) (delete "" (nthcdr 4 words) :test #'string=)))))


(defun edit-records (format)
  (with-temp-file tempname
    (with-open-file (file tempname :direction :output :if-exists :supersede)
      (let ((already-seen (query "SELECT value FROM maintenance ~
                                        WHERE key = 'seen edit message'")))
        (when (or (not already-seen) (verbose format))
          (format file "~

# Here you can edit records' content and tags. You must not edit the
# prefix part of records' header lines: \"# Id: 1 Tags: \". You can edit
# the tag list that comes after the prefix. If record's header spans
# over many lines you must keep the lines together (no empty lines
# between).

# Empty lines at the beginning and end of the record content are
# ignored. If a record has empty content the record will be deleted from
# the database.~%~%")
          (unless already-seen
            (format file "~
# The above message will not show next time unless -v option is used.~%~%")
            (query "INSERT INTO maintenance (key, value) ~
                        VALUES ('seen edit message', 1)")
            (change-counter-add 1))))
      (print-records format file))

    (loop :named editor
          :with hash-table
          := (loop :with table := (make-hash-table :test #'equal)
                   :for record :in (records format)
                   :for i :upfrom 1
                   :do (setf (gethash (hash-record-id i) table) (id record))
                   :finally (return table))
          :with text := (make-array 10 :adjustable t :fill-pointer 0)
          :do
          (run-text-editor tempname)

          (flet ((valid-record-p (id-hash)
                   (and id-hash (nth-value 1 (gethash id-hash hash-table)))))
            (with-open-file (file tempname :direction :input)
              (loop :named content
                    :with start
                    :with end
                    :with last-id-hash := nil
                    :with to-be-deleted := nil
                    :for line := (read-line file nil)
                    :for line-number :upfrom 1

                    :do
                    (multiple-value-bind (id-hash tag-names)
                        (parse-record-header line)

                      (cond
                        ;; This is a record header that adds more tags
                        ;; for the same record.
                        ((and (valid-record-p id-hash)
                              (string= id-hash last-id-hash))
                         (dolist (tag tag-names)
                           (push tag (rest (aref text 0)))))

                        ;; This is a new record header or end-of-file.
                        ((or (valid-record-p id-hash) (not line))
                         (when (integerp start)
                           (let ((record-id (first (aref text 0)))
                                 (tags (rest (aref text 0))))
                             (cond
                               ;; Empty content.
                               ((= start 0)
                                (pushnew record-id to-be-deleted)
                                (message "No content; ~
                                        deleting the record.~%"))
                               ;; There is content.
                               (t
                                (modify-record
                                 record-id
                                 (with-output-to-string (out)
                                   (loop :for i :from start :upto end
                                         :do (write-line (aref text i) out)))
                                 tags)
                                (setf to-be-deleted
                                      (delete record-id to-be-deleted))
                                (message "Updated.~%")))))
                         (unless line
                           ;; Everything done.
                           (dolist (id to-be-deleted)
                             (delete-record id))
                           (message "~&All done.~%")
                           (return-from editor))
                         ;; Let's prepare for parsing a new record.
                         (message "~&Id ~A: " id-hash)
                         (let ((record-id (gethash id-hash hash-table)))
                           (handler-case (assert-tag-names tag-names)
                             (tagdb-error (c)
                               (message "Error!~%")
                               (error-message "~&~A~%" c)
                               (format *query-io* "Press ENTER to return to ~
                                        text editor...")
                               (force-output *query-io*)
                               (read-line *query-io* nil)
                               (fresh-line *query-io*)
                               (return-from content)))
                           (setf (fill-pointer text) 0 start 0 end 0)
                           (vector-push-extend
                            (cons record-id tag-names) text)))

                        ;; Record header look-alike but no valid
                        ;; header seen so far.
                        ((and (not start) id-hash)
                         (error-message "~&Line ~D: The line looks like a ~
                                record header but has an unknown record id.~%~
                                I'm ignoring it because no valid record has ~
                                started in the file yet.~%" line-number))

                        ;; This line is record's content.
                        ((integerp start)
                         (when id-hash
                           (message "Warning!~%")
                           (error-message "~&Line ~D: The line looks like a ~
                                record header but has an unknown record id.~%~
                                I'll take that it's record's content and ~
                                indent the line by two spaces.~%" line-number)
                           (setf line (concatenate 'string "  " line)))
                         (vector-push-extend line text)
                         (cond ((and (= start 0)
                                     (not (empty-string-p line)))
                                (setf start (1- (length text))
                                      end start))
                               ((not (empty-string-p line))
                                (setf end (1- (length text)))))))

                      (setf last-id-hash id-hash))))))))


(defun command-help ()
  (format t "~

Usage: tagdb [options] [--] TAG ...

  The default operation prints all database records that match the given
  TAG(s).

General options

  -q    Quiet output.
  -v    Verbose output.

  --db=FILE
        Use FILE as the database file instead of the default
        ~~/.config/tagdb.db. The program will try to create all the
        necessary directories for FILE.

  --format=MODE
        Set output format to MODE which can be \"text\", \"text-color\"
        or \"org-mode\" (Emacs). You can add suffix \"/default\" to MODE
        in which case the specified mode will be saved as the default
        output mode.

Command options

  -s TAG ...
        Short output. This is like the default operation but only prints
        the first line of records' content. The first line could be used
        as record's title.

  -n TAG ...
        Count and print how many records match the given TAG(s).

  -c TAG ...
        Create a new database record associated with the given tags. If
        there seems to be data coming from the standard input it will be
        saved as the record's content. Otherwise the default text editor
        is launched for editing the record. Empty lines at the beginning
        and end are ignored.

  -e TAG ...
        Find all records that match the given tags and launch the
        default text editor for editing the records' contents and tags.
        Empty lines at the beginning and end of the record content are
        ignored. If a record has empty content the record will be
        deleted from the database.

  -l [STRING]
        List tags that match the given string. If no string is given
        list all tags.

  -r OLD NEW
        Reassociate records. All database records associated with the
        old tag will be associated with the new tag. The old tag is then
        removed.

  -h    Print this help text.

"))


(defun command-create (tag-names)
  (assert-tag-names tag-names)
  (with-transaction
    (assert-db-write-access)
    (if (listen *standard-input*)
        (create-new-record-from-stream tag-names *standard-input*)
        (create-and-edit-new-record tag-names))))


(defun command-edit (format tag-names)
  (assert-tag-names tag-names)
  (setf (records format) (find-records tag-names))
  (with-transaction
    (assert-db-write-access)
    (edit-records format)
    (delete-unused-tags)))


(defun command-list (tag-names)
  (when (rest tag-names)
    (error-message "~&Note: Only the first string is used.~%")
    (setf tag-names (list (first tag-names))))
  (when tag-names
    (assert-tag-names tag-names))
  (print-tags (first tag-names)))


(defun command-number (tag-names)
  (assert-tag-names tag-names)
  (handler-case (format t "~D~%" (length (find-records tag-names)))
    (records-not-found ()
      (format t "0~%"))))


(defun command-reassociate (tag-names)
  (let ((number-of-tags (length tag-names)))
    (case number-of-tags
      (0 (tagdb-error "Must give OLD and NEW tag."))
      (1 (tagdb-error "Must also give NEW tag.")))
    (when (> number-of-tags 2)
      (error-message "~&Only the first two tags are used.~%")
      (setf tag-names (list (first tag-names)
                            (second tag-names))))
    (when (string= (nth 0 tag-names) (nth 1 tag-names))
      (tagdb-error "OLD and NEW tag can't be the same.")))
  (assert-tag-names tag-names)

  (with-transaction
    (assert-db-write-access)
    (let* ((old (nth 0 tag-names))
           (new (nth 1 tag-names))
           (old-id (query-1 "SELECT id FROM tags WHERE name = ~A"
                            (sql-string-esc old)))
           (new-id (query-1 "SELECT id FROM tags WHERE name = ~A"
                            (sql-string-esc new))))

      (cond
        ((not old-id)
         (tagdb-error "Tag \"~A\" not found." old))

        (new-id
         (loop :for changes :upfrom 1
               :for record-id
               :in (query-nconc "SELECT record_id FROM record_tag ~
                                WHERE tag_id = ~D" old-id)
               :do
               (if (query "SELECT * FROM record_tag ~
                                WHERE record_id = ~D AND tag_id = ~D"
                          record-id new-id)
                   (query "DELETE FROM record_tag ~
                                WHERE record_id = ~D AND tag_id = ~D"
                          record-id old-id)
                   (query "UPDATE record_tag SET tag_id = ~D ~
                                WHERE record_id = ~D AND tag_id = ~D"
                          new-id record-id old-id))
               :finally
               (change-counter-add changes)
               (delete-unused-tags)))

        (t (query "UPDATE tags SET name = ~A WHERE id = ~D"
                  (sql-string-esc new) old-id)
           (change-counter-add 1))))))


(defun command-print-records (format tag-names)
  (assert-tag-names tag-names)
  (setf (records format) (find-records tag-names))
  (print-records format))


(defun execute-command-line (args)
  (multiple-value-bind (options other-args unknown)
      (just-getopt-parser:getopt args '((:quiet #\q)
                                        (:verbose #\v)
                                        (:db "db" :required)
                                        (:format "format" :required)
                                        (:short #\s)
                                        (:number #\n)
                                        (:create #\c)
                                        (:edit #\e)
                                        (:list #\l)
                                        (:reassociate #\r)
                                        (:help #\h))
                                 :error-on-unknown-option t
                                 :error-on-argument-missing t)

    (flet ((optionp (id)
             (assoc id options))
           (option-arg (id)
             (cdr (assoc id options))))

      (when (and unknown (not (optionp :help)))
        (tagdb-error "Use option \"-h\" for help."))

      (when (> (length (delete nil (list (optionp :short)
                                         (optionp :number)
                                         (optionp :create)
                                         (optionp :edit)
                                         (optionp :list)
                                         (optionp :reassociate)
                                         (optionp :help))))
               1)
        (tagdb-error "Only one command option is allowed. ~
                        Use option \"-h\" for help."))

      (when (optionp :help)
        (command-help)
        (error 'exit-program))

      (let ((path (option-arg :db)))
        (when path
          (if (plusp (length path))
              (setf *database-pathname* (pathconv:pathname path))
              (tagdb-error "Invalid argument for option \"--db\"."))))

      (with-database
        (let ((format (or (option-arg :format) (get-default-format))))

          (cond ((string= format "text")
                 (setf format 'text))
                ((string= format "text/default")
                 (set-default-format "text")
                 (setf format 'text))
                ((string= format "text-color")
                 (setf format 'text-color))
                ((string= format "text-color/default")
                 (set-default-format "text-color")
                 (setf format 'text-color))
                ((string= format "org-mode")
                 (setf format 'org-mode))
                ((string= format "org-mode/default")
                 (set-default-format "org-mode")
                 (setf format 'org-mode))
                (t (tagdb-error "Invalid argument for option \"--format\".")))

          (cond ((optionp :create) (command-create other-args))
                ((optionp :edit)
                 (command-edit (make-instance 'text-editor
                                              :verbose (optionp :verbose))
                               other-args))
                ((optionp :list) (command-list other-args))
                ((optionp :reassociate) (command-reassociate other-args))
                ((not other-args) (tagdb-error "No tags given."))
                ((optionp :number) (command-number other-args))
                (t
                 (when (and (optionp :quiet) (optionp :verbose))
                   (error-message "~&Option \"-q\" is ignored when ~
                                combined with \"-v\".~%"))
                 (command-print-records
                  (make-instance format
                                 :verbose (optionp :verbose)
                                 :quiet (if (optionp :verbose)
                                            nil
                                            (optionp :quiet))
                                 :short (optionp :short))
                  other-args))))))))


(defun main (&rest args)
  (handler-bind
      ((just-getopt-parser:unknown-option
         (lambda (c)
           (error-message "~A~%" c)
           (invoke-restart 'just-getopt-parser:skip-option)))
       (exit-program
         (lambda (c)
           (declare (ignore c))
           (return-from main))))

    (execute-command-line args)))


(defun start ()
  (handler-case (apply #'main (rest sb-ext:*posix-argv*))
    (sb-int:simple-stream-error ()
      (sb-ext:exit :code 0))
    (sb-sys:interactive-interrupt ()
      (terpri)
      (sb-ext:exit :code 1))
    (error (c)
      (error-message "~&~A~%" c)
      (sb-ext:exit :code 1))))
