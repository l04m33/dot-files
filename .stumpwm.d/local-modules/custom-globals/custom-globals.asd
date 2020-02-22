(asdf:defsystem #:custom-globals
  :serial t
  :description "Custom global availables for StumpWM"
  :author "Kay Z."
  :depends-on (#:stumpwm #:custom-utils)
  :components ((:file "custom-globals")))
