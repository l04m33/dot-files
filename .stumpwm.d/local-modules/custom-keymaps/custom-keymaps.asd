(asdf:defsystem #:custom-keymaps
  :serial t
  :description "Custom keymaps for StumpWM"
  :author "Kay Z."
  :depends-on (#:stumpwm
               #:alexandria
               #:custom-routines
               #:custom-globals
               #:group-set)
  :components ((:file "custom-keymaps")))
