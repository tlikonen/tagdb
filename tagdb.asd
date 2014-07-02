(defsystem :tagdb
  :description "Tag-based command-line database tool"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "The GNU General Public License version 3"
  :depends-on (:sqlite :split-sequence :local-time :unix-options)
  :components ((:file "tagdb")))

#-sbcl (error "Works only with the SBCL implementation.")
(require :sb-posix)
