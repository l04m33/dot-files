(asdf:defsystem #:custom-routines
  :serial t
  :description "Custom functions and commands for StumpWM"
  :author "Kay Z."
  :depends-on (#:stumpwm
               #:custom-globals
               #:swank
               #:vlime)
  :components ((:file "custom-routines")))
