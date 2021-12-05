(defsystem "tagdb"
  :description "Tag-based command-line database tool"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "The GNU General Public License version 3"
  :depends-on ((:require "sb-posix")
               "sqlite" "split-sequence" "local-time" "just-getopt-parser")
  :components ((:file "tagdb" :depends-on ("common" "database" "pathconv"))
               (:file "common")
               (:file "database" :depends-on ("common" "pathconv" "string-io"
                                                       "xdg-dirs"))
               (:file "pathconv")
               (:file "string-io")
               (:file "xdg-dirs" :depends-on ("pathconv"))))
