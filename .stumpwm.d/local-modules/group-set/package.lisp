(defpackage #:group-set
  (:nicknames #:gset)
  (:use #:cl
        #:stumpwm)

  (:export #:group-set

           #:gset-number
           #:gset-name
           #:gset-current-group-nr
           #:gset-groups
           #:gset-screen
           #:gset-current-group

           #:add-group-set
           #:switch-to-group-set
           #:group-gset
           #:current-group-set
           #:find-group-set
           #:move-window-to-group-set))
