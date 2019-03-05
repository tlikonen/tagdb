(load "quicklisp/setup.lisp")

(ql:quickload "tagdb")

(sb-ext:save-lisp-and-die
 "tagdb"
 :executable t
 :toplevel #'tagdb:start
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
