(load "quicklisp/setup.lisp")

(ql:quickload "tagdb")

(sb-ext:save-lisp-and-die
 "tagdb"
 :executable t
 :toplevel (lambda ()
             (apply #'tagdb:main (rest sb-ext:*posix-argv*)))
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
