(asdf:defsystem #:group-set
  :serial t
  :description "Group sets for StumpWM"
  :author "Kay Z."
  :depends-on (#:stumpwm
               #:alexandria)
  :components ((:file "package")
               (:file "group-set")))
