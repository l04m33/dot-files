(defpackage #:custom-globals
  (:nicknames #:cglobal)
  (:use #:cl
        #:stumpwm)

  (:export #:*rc-first-group*
           #:*rc-last-group*
           #:*rc-fonts-dir*
           #:*rc-wp-dir*
           #:*rc-current-wp*
           #:*rc-keyboard-layout*))


(in-package #:custom-globals)


(defparameter *rc-first-group* 0)
(defparameter *rc-last-group*  9)

; ~/.stumpwm.d/fonts/
(defparameter *rc-fonts-dir*
  (cutil:build-resource-dir "fonts"))

; ~/.stumpwm.d/wp/
(defparameter *rc-wp-dir*
  (cutil:build-resource-dir "wp"))

(defparameter *rc-current-wp* nil)

; qwerty or colemak-dh
(defvar *rc-keyboard-layout* 'colemak-dh)
