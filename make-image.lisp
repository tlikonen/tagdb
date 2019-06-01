(require 'asdf)
(asdf:disable-output-translations)
(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory (merge-pathnames "src/"))
       (list :tree (merge-pathnames "quicklisp/dists/"))))

(asdf:load-system "tagdb")

(sb-ext:save-lisp-and-die
 "tagdb"
 :executable t
 :toplevel #'tagdb:start
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
