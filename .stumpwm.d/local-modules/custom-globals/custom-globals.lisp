(defpackage #:custom-globals
  (:nicknames #:cglobal)
  (:use #:cl
        #:stumpwm)

  (:export #:*first-group*
           #:*last-group*
           #:*fonts-dir*
           #:*wp-dir*
           #:*current-wp*))


(in-package #:custom-globals)


(defparameter *first-group* 0)
(defparameter *last-group*  9)

; ~/.stumpwm.d/fonts/
(defparameter *fonts-dir*
  (cutil:build-resource-dir "fonts"))

; ~/.stumpwm.d/wp/
(defparameter *wp-dir*
  (cutil:build-resource-dir "wp"))

(defparameter *current-wp* nil)

