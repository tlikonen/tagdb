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


(defpackage #:database
  (:use #:cl #:common)
  (:export
   #:with-database
   #:query #:query-1 #:query-nconc
   #:with-transaction
   #:query-last-insert-rowid
   #:sql-string-esc #:sql-like-esc
   #:*database-pathname*
   #:change-counter-add
   ))

(in-package #:database)


(defvar *database-pathname* nil)
(defvar *database* nil)
(defvar *changes-before-vacuum* 1000)
(defparameter *program-database-version* 6)


(defun query (format-string &rest parameters)
  (if (typep *database* 'sqlite:sqlite-handle)
      (sqlite:execute-to-list *database*
                              (apply #'format nil format-string parameters))
      (tagdb-error "No connection to the database.")))


(defun query-1 (format-string &rest parameters)
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


(defun sql-like-esc (thing &key wild-start wild-end)
  (with-output-to-string (out)
    (format out "'~A" (if wild-start "%" ""))
    (loop :for char :across (typecase thing
                              (string thing)
                              (character (string thing))
                              (integer (princ-to-string thing))
                              (t ""))
          :do (princ (cond ((char= char #\') "''")
                           ((find char "_%\\") (format nil "\\~A" char))
                           (t char))
                     out))
    (format out "~A' escape '\\'" (if wild-end "%" ""))))


(defun normalize-integer (thing)
  (typecase thing
    (integer thing)
    (string (parse-integer thing :junk-allowed t))))


(defun change-counter-set (value)
  (query "UPDATE maintenance SET value = ~D WHERE key = 'change counter'"
         (normalize-integer value))
  value)


(defun change-counter-get ()
  (normalize-integer (query-1 "SELECT value FROM maintenance ~
                                WHERE key = 'change counter'")))


(defun change-counter-add (count)
  (query "UPDATE maintenance SET value = value + ~D ~
                WHERE key = 'change counter'"
         (normalize-integer count))
  count)


(defun vacuum-check (&optional force)
  (when (or force (>= (change-counter-get) *changes-before-vacuum*))
    (query "VACUUM")
    (change-counter-set 0)
    t))


(defun query-database-version ()
  (let ((value (query-1 "SELECT value FROM maintenance ~
                                WHERE key = 'database version'")))
    (if value (normalize-integer value) 1)))


(defun init-database-pathname ()
  (unless *database-pathname*
    (setf *database-pathname*
          (merge-pathnames (make-pathname :directory '(:relative ".config")
			                  :name "tagdb" :type "db")
                           (user-homedir-pathname))))
  (ensure-directories-exist *database-pathname*))


(defgeneric db-update (version))


(defmethod db-update ((version (eql 2)))
  ;; Add color option.
  (query "INSERT INTO maintenance (key, value) VALUES ('color', 0)")
  (query "UPDATE maintenance SET value = 2 WHERE key = 'database version'"))


(defmethod db-update ((version (eql 3)))
  ;; Use foreign keys in record_tag table.
  (query "PRAGMA foreign_keys = OFF")
  (with-transaction
    (query "CREATE TABLE record_tag_v3 (~
        record_id INTEGER REFERENCES records(id) ON DELETE CASCADE, ~
        tag_id INTEGER REFERENCES tags(id) ON DELETE CASCADE)")
    (query "INSERT INTO record_tag_v3 ~
        SELECT record_id, tag_id FROM record_tag")
    (query "DROP TABLE record_tag")
    (query "ALTER TABLE record_tag_v3 RENAME TO record_tag")
    (query "UPDATE maintenance SET value = 3 WHERE key = 'database version'")))


(defmethod db-update ((version (eql 4)))
  ;; Composite primary key for record_tag table.
  (query "PRAGMA foreign_keys = OFF")
  (with-transaction
    (query "ALTER TABLE record_tag RENAME TO record_tag_old")
    (query "CREATE TABLE record_tag (~
        record_id INTEGER NOT NULL REFERENCES records(id) ON DELETE CASCADE, ~
        tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE, ~
        PRIMARY KEY (record_id, tag_id))")
    (query "INSERT INTO record_tag ~
        SELECT record_id, tag_id FROM record_tag_old")
    (query "DROP TABLE record_tag_old")
    (query "UPDATE maintenance SET value = 4 WHERE key = 'database version'"))
  (query "PRAGMA foreign_keys = ON"))


(defmethod db-update ((version (eql 5)))
  (with-transaction
    (query "UPDATE maintenance SET value = 5 WHERE key = 'database version'")
    (query "PRAGMA auto_vacuum = FULL")))


(defmethod db-update ((version (eql 6)))
  ;; Remove --color option and introduce --format.
  (with-transaction
    (let ((value (query-1 "SELECT value FROM maintenance ~
                                WHERE key = 'color'")))
      (if (eql value 1)
          (query "INSERT INTO maintenance (key, value) ~
                        VALUES ('output format', 'text-color')")
          (query "INSERT INTO maintenance (key, value) ~
                        VALUES ('output format', 'text')")))
    (query "DELETE FROM maintenance WHERE key = 'color'")
    (query "UPDATE maintenance SET value = 6 WHERE key = 'database version'")))


(defun init-database ()
  (if (query-1 "SELECT 1 FROM sqlite_master ~
                WHERE type = 'table' AND name = 'maintenance'")
      (let ((version (query-database-version)))
        (cond ((< version *program-database-version*)
               (error-message "Updating database from v~D to v~D.~%"
                              version *program-database-version*)
               (loop :for target :from (1+ version)
                     :upto *program-database-version*
                     :do (db-update target))
               (vacuum-check t))
              ((> version *program-database-version*)
               (tagdb-error "The database is of version ~D ~
                but this program can only handle versions upto ~D.~%~
                Please update the program."
                            version *program-database-version*))))

      ;; Database is missing
      (with-transaction
        (message "~&Preparing database file \"~A\".~%"
                 (pathconv:namestring *database-pathname*))

        (query "PRAGMA auto_vacuum = FULL")

        (query "CREATE TABLE maintenance (~
                key TEXT UNIQUE, ~
                value INTEGER)")

        (query "INSERT INTO maintenance (key, value) VALUES ~
                ('database version', ~D)" *program-database-version*)
        (query "INSERT INTO maintenance (key, value) ~
                VALUES ('change counter', 0)")
        (query "INSERT INTO maintenance (key, value) ~
                VALUES ('output format', 'text')")

        (query "CREATE TABLE records (~
                id INTEGER PRIMARY KEY, ~
                created INTEGER, ~
                modified INTEGER, ~
                content TEXT)")

        (query "CREATE TABLE tags (~
                id INTEGER PRIMARY KEY, ~
                name TEXT UNIQUE)")

        (query "CREATE TABLE record_tag (~
        record_id INTEGER NOT NULL REFERENCES records(id) ON DELETE CASCADE, ~
        tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE, ~
        PRIMARY KEY (record_id, tag_id))")))

  (query "PRAGMA foreign_keys = ON")
  (query "PRAGMA case_sensitive_like = ON"))


(defun connect ()
  (unless (typep *database* 'sqlite:sqlite-handle)
    (init-database-pathname)
    (setf *database* (sqlite:connect (pathconv:namestring
                                      *database-pathname*)))
    (init-database)
    *database*))


(defun disconnect ()
  (when (typep *database* 'sqlite:sqlite-handle)
    (prog1 (sqlite:disconnect *database*)
      (setf *database* nil))))


(defmacro with-database (&body body)
  `(let ((*database* nil))
     (unwind-protect (multiple-value-prog1
                         (progn (connect) ,@body)
                       (vacuum-check))
       (disconnect))))
