;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:fstools
  (:use #:cl)
  (:export #:file-type #:file-type-p #:get-umask
           #:with-temp-file #:copy-file #:move-file))

(in-package #:fstools)

(defun file-type (pathspec &key follow-symlinks)
  (let ((mode (sb-posix:stat-mode
               (if follow-symlinks
                   (sb-posix:stat (pathconv:namestring pathspec))
                   (sb-posix:lstat (pathconv:namestring pathspec))))))
    (loop :for (type . keyword)
          :in '((#.sb-posix:s-ifsock . :socket)
                (#.sb-posix:s-iflnk  . :symbolic-link)
                (#.sb-posix:s-ifreg  . :regular-file)
                (#.sb-posix:s-ifblk  . :block-device)
                (#.sb-posix:s-ifdir  . :directory)
                (#.sb-posix:s-ifchr  . :character-device)
                (#.sb-posix:s-ififo  . :fifo))
          :if (= type (logand type mode))
          :return keyword)))

(defun file-type-p (pathspec type &key follow-symlinks)
  (eql (file-type pathspec :follow-symlinks follow-symlinks) type))

(defun get-umask ()
  (let ((mode (sb-posix:umask 0)))
    (sb-posix:umask mode)
    mode))

(defmacro with-temp-file (file &body body)
  `(let ((,file (pathconv:pathname
                 (nth-value 1 (sb-posix:mkstemp "/tmp/tmp-XXXXXX")))))
     (unwind-protect (progn ,@body)
       (delete-file ,file))))

(defun copy-file (from to)
  (assert (file-type-p from :regular-file)
          nil "The FROM file must be a regular file.")

  (with-open-file (out (pathconv:pathname to)
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)

    (with-open-file (in (pathconv:pathname from)
                        :direction :input
                        :element-type '(unsigned-byte 8))

      (loop
        :with size := (let ((bf #. (* 1024 1024))
                            (len (file-length in)))
                        (if len (min len bf) bf))
        :with buffer := (make-array size :element-type '(unsigned-byte 8))
        :for n := (read-sequence buffer in :start 0
                                           :end size)
        :while (plusp n)
        :do (write-sequence buffer out :start 0 :end n)))))

(defun move-file (from to)
  (copy-file from to)
  (delete-file (pathconv:pathname from)))
