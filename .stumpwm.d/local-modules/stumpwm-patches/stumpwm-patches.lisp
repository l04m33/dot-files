(in-package #:stumpwm)

;; The CLEAR-FRAME-OUTLINES function in StumpWM source has :EXPOSURES-P set to
;; NIL, and fails to actually clear the frame outlines. Setting :EXPOSURES-P to
;; T solved the problem.
(defun clear-frame-outlines (group)
  (xlib:clear-area (screen-root (group-screen group)) :exposures-p t))

