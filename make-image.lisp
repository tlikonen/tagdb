;;; Copyright (C) 2015 Teemu Likonen <tlikonen@iki.fi>
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


(defparameter *system* (nth 1 sb-ext:*posix-argv*))
(defparameter *image* (nth 2 sb-ext:*posix-argv*))

(when (or (not *image*)
          (zerop (length *image*)))
  (write-line "Image file name missing." *error-output*)
  (sb-ext:exit :code 1))

(format t "Creating SBCL image file: ~A~%~A ~A~%~%"
        *image*
        (lisp-implementation-type)
        (lisp-implementation-version))

(handler-case
    (let ((home (user-homedir-pathname))
          (pwd *default-pathname-defaults*))
      (flet ((probe-load (path &optional (default home))
               (let ((path (merge-pathnames path default)))
                 (when (probe-file path) (load path))))
             (funcallstr (string &rest args)
               (apply (read-from-string string) args)))
        (or (probe-load #p"quicklisp/setup.lisp" home)
            (probe-load #p".quicklisp/setup.lisp" home)
            (probe-load #p"quicklisp/setup.lisp" pwd)
            (let ((init (nth-value 1 (progn
                                       (require :sb-posix)
                                       (funcallstr "sb-posix:mkstemp"
                                                   "/tmp/quicklisp-XXXXXX"))))
                  (url "http://beta.quicklisp.org/quicklisp.lisp"))
              (unwind-protect
                   (progn
                     (sb-ext:run-program "wget" (list "-O" init url)
                                         :search t :output t)
                     (when (probe-load init pwd)
                       (funcallstr
                        "quicklisp-quickstart:install"
                        :path (merge-pathnames #p"quicklisp/" pwd))))
                (delete-file init))))))

  (sb-sys:interactive-interrupt ()
    (sb-ext:exit :code 1))
  (error (c)
    (format *error-output* "~&~A~%" c)
    (sb-ext:exit :code 1)))

(handler-case (ql:quickload *system*)
  (sb-sys:interactive-interrupt ()
    (sb-ext:exit :code 1))
  (error (c)
    (format *error-output* "~&~A~%" c)
    (sb-ext:exit :code 1)))

(sb-ext:save-lisp-and-die
 *image*
 :executable t
 :toplevel (lambda ()
             (tagdb:main sb-ext:*posix-argv*))
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
