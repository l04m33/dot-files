(asdf:defsystem #:useless-gaps
  :serial t
  :description "Useless gaps for StumpWM"
  :author "vlnx <https://github.com/vlnx>"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "useless-gaps")))

