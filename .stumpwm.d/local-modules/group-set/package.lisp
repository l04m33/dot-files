(defpackage #:group-set
  (:nicknames #:gset)
  (:use #:cl
        #:stumpwm)

  (:export #:group-set

           #:gset-number
           #:gset-name
           #:gset-current-group
           #:gset-groups
           #:gset-screen

           #:add-group-set
           #:switch-to-group-set
           #:group-gset
           #:current-group-set
           #:find-group-set
           #:move-window-to-group-set))
