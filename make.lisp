(defparameter *sbcl* (nth 0 sb-ext:*posix-argv*))
(defparameter *lib* (sb-ext:native-pathname (nth 1 sb-ext:*posix-argv*)))

(load "asdf.conf")
(load "quicklisp/setup.lisp")
(ql:quickload "tagdb")

(asdf:operate 'asdf:monolithic-deliver-asd-op "tagdb")

(with-open-file (f "build/tagdb" :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-right-margin* 78)
          (*print-case* :downcase))
      (format f "#!~A --script~%~@{~S~%~}"
              *sbcl*
              '(require "sb-posix")
              '(require "asdf")
              (list 'asdf:initialize-source-registry
                    (list 'quote
                          (list :source-registry
                                :ignore-inherited-configuration
                                (list :directory *lib*))))
              '(handler-case
                (asdf:operate 'asdf:monolithic-load-bundle-op "tagdb")
                (serious-condition (c)
                 (format *error-output* "~A~%" c)
                 (sb-ext:exit :code 1)))
              '(tagdb:start)))))

(with-open-file (*standard-output* "build/help.txt"
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
  (tagdb:command-help))
