(defsystem "tagdb"
  :description "Tag-based command-line database tool"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "The GNU General Public License version 3"
  :depends-on ("sqlite" "split-sequence" "local-time" "just-getopt-parser")
  :components ((:file "tagdb" :depends-on ("pathconv"))
               (:file "pathconv")))
