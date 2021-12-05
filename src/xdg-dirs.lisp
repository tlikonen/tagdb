;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:xdg-dirs
  (:use #:cl)
  (:export
   #:data-home #:config-home #:cache-home #:state-home))

(in-package #:xdg-dirs)

(defun getenv (var)
  "Return system environment variable VAR.
If the variable is not defined return nil."
  (sb-posix:getenv var))

(defun xdg-query (variable default-dir path)
  (setf path (pathconv:pathname path))
  (let ((env (getenv variable)))
    (if env
        (merge-pathnames path (pathconv:pathname env :type :directory))
        (merge-pathnames path (merge-pathnames default-dir
                                               (user-homedir-pathname))))))

(defun data-home (&optional (path #p""))
  "Merge pathname $XDG_DATA_HOME with PATH."
  (xdg-query "XDG_DATA_HOME" #p".local/share/" path))

(defun config-home (&optional (path #p""))
  "Merge pathname $XDG_CONFIG_HOME with PATH."
  (xdg-query "XDG_CONFIG_HOME" #p".config/" path))

(defun cache-home (&optional (path #p""))
  "Merge pathname $XDG_CACHE_HOME with PATH."
  (xdg-query "XDG_CACHE_HOME" #p".cache/" path))

(defun state-home (&optional (path #p""))
  "Merge pathname $XDG_STATE_HOME with PATH."
  (xdg-query "XDG_STATE_HOME" #p".local/state/" path))
