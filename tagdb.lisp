;;;; Tagdb --- Tag-based command-line database tool


;;; Copyright (C) 2014 Teemu Likonen <tlikonen@iki.fi>
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
  (:use #:cl)
  (:import-from #:split-sequence #:split-sequence)
  (:export #:main))

(in-package #:tagdb)


(defvar *database-pathname* nil)
(defvar *database* nil)
(defvar *changes-before-vacuum* 1000)
(defvar *output-quiet* nil)
(defvar *output-short* nil)
(defvar *output-verbose* nil)
(defvar *output-editor* nil)
(defparameter *program-database-version* 1)


(define-condition exit-program () nil)


(define-condition tagdb-error (error)
  ((text :reader tagdb-error-text :initarg :text))
  (:report (lambda (condition stream)
             (format stream "~A" (tagdb-error-text condition)))))


(defun throw-error (fmt &rest args)
  (error 'tagdb-error :text (apply #'format nil fmt args)))


(defun message (fmt &rest args)
  (unless *output-quiet*
    (apply #'format t fmt args)
    (force-output)))


(defun error-message (fmt &rest args)
  (apply #'format *error-output* fmt args)
  (force-output *error-output*))


(defun query (format-string &rest parameters)
  (if (typep *database* 'sqlite:sqlite-handle)
      (sqlite:execute-to-list *database*
                              (apply #'format nil format-string parameters))
      (throw-error "No connection to the database.")))


(defun query-caar (format-string &rest parameters)
  (caar (apply #'query format-string parameters)))


(defun query-nconc (format-string &rest parameters)
  (reduce #'nconc (apply #'query format-string parameters)))


(defmacro with-transaction (&body body)
  `(sqlite:with-transaction *database* ,@body))


(defun query-last-insert-rowid ()
  (sqlite:last-insert-rowid *database*))


(defun sql-string-esc (thing)
  (with-output-to-string (out)
    (princ #\' out)
    (loop :for char :across (typecase thing
                              (string thing)
                              (character (string thing))
                              (integer (princ-to-string thing))
                              (t ""))
          :do (princ (if (char= char #\') "''" char) out))
    (princ #\' out)))


(defun sql-like-esc (str &optional (start "") (end ""))
  (with-output-to-string (out)
    (format out "'~A" start)
    (loop :for char :across (typecase str
                              (string str)
                              (character (string str))
                              (integer (princ-to-string str))
                              (t ""))
          :do (princ (cond ((char= char #\') "''")
                           ((find char "_%\\") (format nil "\\~A" char))
                           (t char))
                     out))
    (format out "~A' escape '\\'" end)))


(defun normalize-integer (thing)
  (typecase thing
    (integer thing)
    (string (parse-integer thing :junk-allowed t))))


(defun change-counter-set (value)
  (query "UPDATE maintenance SET value=~A WHERE key='change counter'"
         (normalize-integer value))
  value)


(defun change-counter-get ()
  (normalize-integer (query-caar "SELECT value FROM maintenance ~
                                WHERE key='change counter'")))


(defun change-counter-add (count)
  (change-counter-set (+ count (change-counter-get))))


(defun vacuum-maybe (&optional force)
  (when (or force (>= (change-counter-get) *changes-before-vacuum*))
    (query "VACUUM")
    (change-counter-set 0)
    t))


(defun query-database-version ()
  (let ((value (query-caar "SELECT value FROM maintenance ~
                                WHERE key='database version'")))
    (if value (normalize-integer value) 1)))


(defun init-database-pathname ()
  (unless *database-pathname*
    (setf *database-pathname*
          (make-pathname
           :directory (append (pathname-directory (user-homedir-pathname))
                              '(".config"))
           :name "tagdb" :type "db")))
  (ensure-directories-exist *database-pathname*))


(defun init-database ()
  (let ((all (query-nconc "SELECT name FROM sqlite_master WHERE type='table'")))
    (flet ((table-exists-p (thing)
             (member thing all :test #'string-equal)))

      (if (table-exists-p "maintenance")
          (let ((version (query-database-version)))
            (cond ((= version *program-database-version*)
                   ;; Everything OK.
                   )
                  ((> version *program-database-version*)
                   (throw-error "Database file is of version ~A ~
                but this program can only handle versions upto ~A.~%~
                Please update the program."
                                version *program-database-version*))))

          ;; Database is missing
          (with-transaction
            (message "~&Preparing database file ~A.~%"
                     (sb-ext:native-pathname *database-pathname*))
            (query "CREATE TABLE maintenance (key TEXT UNIQUE, value INTEGER)")
            (query "INSERT INTO maintenance (key, value) VALUES ~
                ('database version',~A)" *program-database-version*)
            (query "INSERT INTO maintenance (key, value) ~
                VALUES ('change counter', 0)")
            (query "CREATE TABLE records (id INTEGER PRIMARY KEY, ~
                        created INTEGER, ~
                modified INTEGER, content TEXT)")
            (query "CREATE TABLE tags (id INTEGER PRIMARY KEY, ~
                        name TEXT UNIQUE)")
            (query "CREATE TABLE record_tag (record_id INTEGER, ~
                        tag_id INTEGER)")))

      (query "PRAGMA case_sensitive_like=1"))))


(defun connect ()
  (unless (typep *database* 'sqlite:sqlite-handle)
    (init-database-pathname)
    (setf *database* (sqlite:connect *database-pathname*))
    (init-database)
    *database*))


(defun disconnect ()
  (when (typep *database* 'sqlite:sqlite-handle)
    (prog1 (sqlite:disconnect *database*)
      (setf *database* nil))))


(defmacro with-database (&body body)
  `(let ((*database* nil))
     (unwind-protect
          (progn (connect) (sqlite:with-transaction *database* ,@body))
       (vacuum-maybe)
       (disconnect))))


(defun valid-tag-name-p (tag-name)
  (and (plusp (length tag-name))
       (every (lambda (char)
                (and (not (eql #\space char))
                     (graphic-char-p char)))
              tag-name)))


(defun delete-unused-tags (&optional tag-ids)
  (unless tag-ids
    (setf tag-ids (query-nconc "SELECT id FROM tags")))
  (loop :for id :in tag-ids
        :unless (query "SELECT tag_id FROM record_tag WHERE tag_id=~A" id)
        :do
        (query "DELETE FROM tags WHERE id=~A" id)
        (change-counter-add 1)
        :and :collect id))


(defun db-insert-record (text)
  (let ((now (get-universal-time)))
    (query "INSERT INTO records (created, modified, content) ~
                VALUES (~A, ~A, ~A)"
           now now (sql-string-esc text))
    (prog1 (query-last-insert-rowid)
      (change-counter-add 1))))


(defun db-modify-record (record-id text)
  (query "UPDATE records SET modified=~A,content=~A WHERE id=~A"
         (get-universal-time) (sql-string-esc text) record-id)
  (change-counter-add 1)
  record-id)


(defun db-delete-record (record-id)
  (query "DELETE FROM records WHERE id=~A" record-id)
  (change-counter-add 1)
  record-id)


(defun db-insert-tag (tag-name)
  (query "INSERT INTO tags (name) VALUES (~A)" (sql-string-esc tag-name))
  (prog1 (query-last-insert-rowid)
    (change-counter-add 1)))


(defun insert-or-get-tag (tag-name)
  (let ((id (query-caar "SELECT id FROM tags WHERE name=~A"
                        (sql-string-esc tag-name))))
    (if id id (prog1 (db-insert-tag tag-name)
                (change-counter-add 1)))))


(defun db-modify-record-tag-connection (record-id new-tag-ids)
  (setf new-tag-ids (remove-duplicates new-tag-ids))
  (let ((old-tag-ids (query-nconc "SELECT tag_id FROM record_tag ~
                                                WHERE record_id=~A"
                                  record-id)))

    (let ((diff (set-difference old-tag-ids new-tag-ids)))
      (when diff
        (query "DELETE FROM record_tag ~
                WHERE record_id=~A AND (~{tag_id=~A~^ OR ~})"
               record-id diff)
        (delete-unused-tags diff)
        (change-counter-add (length diff))))

    (loop :with new := (set-difference new-tag-ids old-tag-ids)
          :for tag-id :in new
          :do (query "INSERT INTO record_tag (record_id, tag_id) ~
                        VALUES (~A, ~A)" record-id tag-id)
          :finally (change-counter-add (length new)))

    (values new-tag-ids old-tag-ids)))


(defun assert-tag-names (tag-names)
  (unless tag-names
    (throw-error "No tags. At least one tag is required."))
  (loop :for tag-name :in (etypecase tag-names
                            (list tag-names)
                            (string (list tag-names)))
        :unless (valid-tag-name-p tag-name)
        :do (throw-error "\"~A\" is not a valid tag name." tag-name)
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
  (let ((tag-ids (query-nconc "SELECT tag_id FROM record_tag ~
                                        WHERE record_id=~A" record-id)))
    (db-delete-record record-id)
    (query "DELETE FROM record_tag WHERE record_id=~A" record-id)
    (change-counter-add 1)
    (delete-unused-tags tag-ids)
    record-id))


(defun find-records (tag-names)
  (let ((record-ids nil)
        (tags nil)
        (records-error-msg "No records found."))

    (unless (setf record-ids (query-nconc "SELECT j.record_id ~
                        FROM record_tag AS j ~
                        LEFT JOIN tags AS t ON j.tag_id=t.id ~
                        WHERE ~{t.name LIKE ~A~^ OR ~}"
                                          (mapcar (lambda (tag)
                                                    (sql-like-esc tag "%" "%"))
                                                  tag-names)))
      (throw-error records-error-msg))

    (loop :for record-id :in record-ids
          :for record-tag-names := (query-nconc "SELECT t.name ~
                        FROM record_tag AS j ~
                        LEFT JOIN tags AS t ON j.tag_id=t.id ~
                        WHERE j.record_id=~A" record-id)
          :if (every (lambda (tag)
                       (member tag record-tag-names :test #'search))
                     tag-names)
          :collect (cons record-id (sort record-tag-names #'string-lessp))
          :into collection
          :finally (unless (setf tags (delete-duplicates collection
                                                         :key #'first))
                     (throw-error records-error-msg)))

    (loop :for (id . names) :in tags
          :for record-data := (first (query "SELECT ~
                                id,created,modified,content ~
                                FROM records WHERE id=~A" id))
          :collect (nconc record-data names) :into collection
          :finally (return (sort collection #'string-lessp
                                 :key (lambda (item)
                                        (format nil "~{~A ~}"
                                                (nthcdr 4 item))))))))


(defun hash-record-id (id)
  (sxhash (princ-to-string id)))


(defun format-time (universal-time)
  (local-time:format-timestring
   nil (local-time:universal-to-timestamp universal-time)
   :format '(:year "-" (:month 2) "-" (:day 2)
             " " (:hour 2) ":" (:min 2) ":" (:sec 2) " " :gmt-offset)))


(defun print-records (record-lists &optional (stream *standard-output*))
  (loop :with formatter
        := (cond
             (*output-editor*
              (formatter "~&# Id: ~A~2* Tags: ~{~A~^ ~}~%~%~A~&"))
             ((and *output-verbose* *output-short*)
              (formatter "~&~*# Created:  ~A~%~
                        # Modified: ~A~%~
                        # Tags: ~{~A~^ ~}~%~*"))
             (*output-short*
              (formatter "~&~3*# Tags: ~{~A~^ ~}~*"))
             (*output-verbose*
              (formatter "~&~*# Created:  ~A~%~
                        # Modified: ~A~%~
                        # Tags: ~{~A~^ ~}~%~%~A~&"))
             (*output-quiet*
              (formatter "~&~4*~A~&"))
             (t (formatter "~&~3*# Tags: ~{~A~^ ~}~%~%~A~&")))

        :for (now . rest) :on record-lists
        :for (id created modified content . tag-names) := now
        :do (funcall formatter stream (hash-record-id id)
                     (format-time created) (format-time modified)
                     tag-names content)
        :if rest :do (terpri stream)
        :finally (fresh-line stream)))


(defun db-find-tags (&optional tag-name)
  (sort (query-nconc "SELECT name FROM tags WHERE name LIKE ~A"
                     (sql-like-esc tag-name "%" "%"))
        #'string-lessp))


(defun print-tags (&optional tag-name)
  (let ((tags (db-find-tags tag-name))
        (*output-quiet* nil))
    (if tags
        (loop :with columns := 78
              :with column := 0
              :for tag :in tags
              :do (cond ((or (= column 0)
                             (> (+ column 1 (length tag)) columns))
                         (message "~&~A" tag)
                         (setf column (length tag)))
                        (t
                         (message " ~A" tag)
                         (incf column (1+ (length tag)))))
              :finally (message "~&"))
        (throw-error "No tags found."))))


(defmacro with-temp-file (file &body body)
  (let ((name (gensym "FILENAME")))
    `(handler-case
         (let* ((,name (sb-ext:parse-native-namestring
                        (nth-value 1 (sb-posix:mkstemp "/tmp/tagdb-XXXXXX"))))
                (,file ,name))
           (unwind-protect (progn ,@body)
             (delete-file ,name)))
       (sb-posix:syscall-error ()
         (throw-error "Couldn't create a temporary file.")))))


(defun run-text-editor (pathname)
  (sb-ext:run-program
   (let ((editor (sb-posix:getenv "EDITOR")))
     (cond ((and (stringp editor)
                 (plusp (length editor)))
            editor)
           (t (throw-error "Please set $EDITOR variable."))))
   (list (etypecase pathname
           (pathname (sb-ext:native-namestring pathname :as-file t))
           (string pathname)))
   :search t :wait t :pty nil :input t :output t :error :output))


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
            (throw-error "Empty file. Aborting."))))


(defun create-and-edit-new-record (tag-names)
  (with-temp-file tempname
    (run-text-editor tempname)
    (with-open-file (file tempname :direction :input)
      (create-new-record-from-stream tag-names file))))


(defun parse-record-header (line)
  (let ((words (split-sequence #\space line :remove-empty-subseqs nil)))
    (when (and (equal "#" (nth 0 words))
               (equal "Id:" (nth 1 words))
               (every #'digit-char-p (nth 2 words))
               (equal "Tags:" (nth 3 words)))
      (values (normalize-integer (nth 2 words))
              (delete "" (nthcdr 4 words) :test #'string=)))))


(defun find-and-edit-records (tag-names)
  (let ((records (find-records tag-names)))

    (with-temp-file tempname
      (with-open-file (file tempname :direction :output :if-exists :overwrite)
        (let ((already-seen (query "SELECT value FROM maintenance ~
                                        WHERE key='seen edit message'")))
          (when (or (not already-seen) *output-verbose*)
            (format file "~
# Here you can edit records' content and tags. You must not edit record
# header lines other than the tag list. Empty lines at the beginning and
# end of the record content are ignored. If record's content is
# completely empty (no lines or only empty lines) the record will be
# deleted.~%~%")
            (unless already-seen
              (format file "~
# The above message will not show next time unless -v option is used.~%~%")
              (query "INSERT INTO maintenance (key, value) ~
                        VALUES ('seen edit message', 1)")
              (change-counter-add 1))))
        (let ((*output-editor* t))
          (print-records records file)))

      (loop :named editor
            :with hash-table
            := (loop :with table := (make-hash-table)
                     :for (id . nil) :in records
                     :do (setf (gethash (hash-record-id id) table) id)
                     :finally (return table))
            :with text := (make-array 10 :adjustable t :fill-pointer 0)
            :do
            (run-text-editor tempname)

            (with-open-file (file tempname :direction :input)
              (loop :named content
                    :with start
                    :with end
                    :for line := (read-line file nil)

                    :do
                    (multiple-value-bind (id-hash tag-names)
                        (parse-record-header line)

                      (cond
                        ((or (nth-value 1 (gethash id-hash hash-table))
                             (not line))
                         (when (integerp start)
                           (let ((record-id (first (aref text 0)))
                                 (tags (rest (aref text 0))))
                             (if (= start 0)
                                 (progn
                                   (delete-record record-id)
                                   (message "No content. ~
                                        The record is deleted.~%"))
                                 (progn
                                   (modify-record
                                    record-id
                                    (with-output-to-string (out)
                                      (loop :for i :from start :upto end
                                            :do (write-line (aref text i) out)))
                                    tags)
                                   (message "Updated.~%")))))
                         (unless line
                           (message "~&All done.~%")
                           (return-from editor))
                         (message "~&Processing record ~A: " id-hash)
                         (let ((record-id (gethash id-hash hash-table)))
                           (handler-case (assert-tag-names tag-names)
                             (tagdb-error (c)
                               (message "~%")
                               (error-message "~&~A Returning to editor ~
                                        in 5 seconds..." c)
                               (sleep 5)
                               (error-message "~%")
                               (return-from content)))
                           (setf (fill-pointer text) 0 start 0 end 0)
                           (vector-push-extend
                            (cons record-id tag-names) text)))

                        ((and (not start) id-hash)
                         (error-message "~&There is a line that looks like a ~
                                record header but has an unknown record id.~%~
                                I'm ignoring it because no valid record has ~
                                started in the file yet.~%"))

                        ((integerp start)
                         (when id-hash
                           (message "Warning!~%")
                           (error-message "~&There is a line that looks like a ~
                                record header but has an unknown record id.~%~
                                I'll take that it's record's content and ~
                                indent the line by two spaces.~%")
                           (setf line (concatenate 'string "  " line)))
                         (vector-push-extend line text)
                         (cond ((and (= start 0)
                                     (not (empty-string-p line)))
                                (setf start (1- (length text))
                                      end start))
                               ((not (empty-string-p line))
                                (setf end (1- (length text))))))))))))))


(defun command-h ()
  (format t "~&~
Tagdb is a simple tag-based database tool which can store any kind of
text records. Every record is associated with one or more tags which can
be used to find the records.

Usage: tagdb [options] [--] [tag ...]

General options

  -q    Quiet output.
  -v    Verbose output.

By default the program prints database records that match the given
tags. There are also options for other operations which are mutually
exclusive:

  -s <tag ...>

        Short output. This is like the default operation but does not
        print records' content, only tags.

  -c <tag ...>

        Create a new database record associated with the given tags. If
        there seems to be data coming from the standard input it will be
        saved as the record's content. Otherwise the default text editor
        is launched for editing the record's content. Empty lines at the
        beginning and end of the record content are ignored.

  -e <tag ...>

        Find all records that match the given tags and launch the
        default text editor for editing the records' contents and tags.
        When the editor has quit the records will be updated. Empty
        lines at the beginning and end of the record content are
        ignored. If record's content is completely empty (no lines or
        only empty lines) the record will be deleted.

  -l [tag]

        List tags that match the given tag. If no tag is given list all
        tags.

  -r <old tag> <new tag>

        Re-associate records. All database records associated with the
        old tag will be associated with the new tag. The old tag will
        disappear.

  -h    Print this help text.

"))


(defun command-c (tag-names)
  (assert-tag-names tag-names)
  (with-database
    (if (listen *standard-input*)
        (create-new-record-from-stream tag-names *standard-input*)
        (create-and-edit-new-record tag-names))))


(defun command-e (tag-names)
  (assert-tag-names tag-names)
  (with-database
    (find-and-edit-records tag-names)))


(defun command-l (tag-names)
  (when (rest tag-names)
    (error-message "~&Only the first tag is used.~%")
    (setf (rest tag-names) nil))
  (when tag-names
    (assert-tag-names tag-names))
  (with-database
    (print-tags (first tag-names))))


(defun command-r (tag-names)
  (let ((number-of-tags (length tag-names)))
    (case number-of-tags
      (0 (throw-error "Must give <old tag> and <new tag>."))
      (1 (throw-error "Must also give <new tag>.")))
    (when (> number-of-tags 2)
      (error-message "~&Only the first two tags are used.~%")
      (setf (rest (rest tag-names)) nil))
    (when (equal (nth 0 tag-names) (nth 1 tag-names))
      (throw-error "<old tag> and <new tag> can't be the same.")))
  (assert-tag-names tag-names)

  (with-database
    (let* ((old (nth 0 tag-names))
           (new (nth 1 tag-names))
           (old-id (query-caar "SELECT id FROM tags WHERE name=~A"
                               (sql-string-esc old)))
           (new-id (query-caar "SELECT id FROM tags WHERE name=~A"
                               (sql-string-esc new))))

      (if (not old-id)
          (throw-error "Tag \"~A\" not found." old)
          (if new-id
              (loop :for changes :upfrom 1
                    :for record-id
                    :in (query-nconc "SELECT record_id FROM record_tag ~
                                WHERE tag_id=~A" old-id)
                    :do
                    (if (query "SELECT * FROM record_tag ~
                                WHERE record_id=~A AND tag_id=~A"
                               record-id new-id)
                        (query "DELETE FROM record_tag ~
                                WHERE record_id=~A AND tag_id=~A"
                               record-id old-id)
                        (query "UPDATE record_tag SET tag_id=~A ~
                                WHERE record_id=~A"
                               new-id record-id))
                    :finally
                    (change-counter-add changes)
                    (delete-unused-tags (list old-id)))

              (progn
                (query "UPDATE tags SET name=~A WHERE id=~A"
                       (sql-string-esc new) old-id)
                (change-counter-add 1)))))))


(defun command-print-records (tag-names)
  (assert-tag-names tag-names)
  (with-database
    (print-records (find-records tag-names))))


(let ((general-options "qv")
      (command-options "scelrh"))

  (defun parse-command-line (arglist)
    (multiple-value-bind (ignored options other)
        (handler-case
            (unix-options:getopt arglist (concatenate 'string general-options
                                                      command-options)
                                 nil)
          (type-error ()
            (throw-error "Couldn't parse the command-line.~%~
                The pseudo option \"--\" marks the end of options ~
                (and the start of tag list).")))
      (declare (ignore ignored))
      (values (delete-duplicates options :test #'equal) other)))


  (defun execute-command-line (arglist)
    (multiple-value-bind (options tag-names)
        (parse-command-line arglist)

      (flet ((optionp (option)
               (and (member option options :test #'equal)
                    (or (find (aref option 0) general-options :test #'equal)
                        (loop :for o :across command-options
                              :for string-o := (string o)
                              :do (when (and (not (equal string-o option))
                                             (find string-o options
                                                   :test #'equal))
                                    (throw-error "Mutually exclusive options ~
                                        used together. Use -h for help."))
                              :finally (return t))))))

        (let ((*output-quiet* (optionp "q"))
              (*output-verbose* (optionp "v"))
              (*output-short* (optionp "s")))

          (cond ((optionp "h") (command-h))
                ((optionp "c") (command-c tag-names))
                ((optionp "e") (command-e tag-names))
                ((optionp "l") (command-l tag-names))
                ((optionp "r") (command-r tag-names))
                ((not tag-names) (throw-error "No tags given."))
                (t
                 (when (and *output-quiet* *output-short*)
                   (error-message "~&Option \"-q\" is ignored when ~
                                combined with \"-s\".~%"))
                 (when (and *output-quiet* *output-verbose*)
                   (error-message "~&Option \"-q\" is ignored when ~
                                combined with \"-v\".~%"))
                 (command-print-records tag-names))))))))


(defun main (&optional argv)
  (handler-case (execute-command-line (rest argv))
    (exit-program () nil)
    (error (c)
      (error-message "~&~A~%" c))
    (sb-sys:interactive-interrupt ()
      (format t "~%"))))


#+script
(main sb-ext:*posix-argv*)
