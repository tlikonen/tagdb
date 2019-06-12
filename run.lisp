;; sbcl --script run.lisp ...

(require "asdf")
(require "sb-posix")

(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory (merge-pathnames "build/src/"))))

(handler-case
    (asdf:operate 'asdf:monolithic-load-bundle-op "tagdb")
  (serious-condition (c)
    (format *error-output* "~A~%" c)
    (sb-ext:exit :code 1)))

(tagdb:start)
