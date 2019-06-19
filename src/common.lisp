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


(defpackage #:common
  (:use #:cl)
  (:export
   #:exit-program
   #:tagdb-error #:tagdb-error-text
   #:records-not-found
   #:message #:error-message
   ))

(in-package #:common)


(define-condition exit-program () nil)


(define-condition tagdb-error (error)
  ((text :type string :reader tagdb-error-text :initarg :text))
  (:report (lambda (condition stream)
             (format stream "~A" (tagdb-error-text condition)))))


(define-condition records-not-found (tagdb-error)
  nil
  (:report "Records not found."))


(defun tagdb-error (fmt &rest args)
  (error 'tagdb-error :text (apply #'format nil fmt args)))


(defun message (fmt &rest args)
  (apply #'format t fmt args)
  (force-output))


(defun error-message (fmt &rest args)
  (apply #'format *error-output* fmt args)
  (force-output *error-output*))
