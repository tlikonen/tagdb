(require 'asdf)
(asdf:disable-output-translations)
(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory (merge-pathnames "src/"))))

(load "quicklisp/setup.lisp")
(ql:quickload "tagdb")
