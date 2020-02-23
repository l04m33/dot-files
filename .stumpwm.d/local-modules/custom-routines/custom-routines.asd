(asdf:defsystem #:custom-routines
  :serial t
  :description "Custom functions and commands for StumpWM"
  :author "Kay Z."
  :depends-on (#:stumpwm)
  :components ((:file "custom-routines")))
