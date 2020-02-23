(defpackage #:custom-routines
  (:nicknames #:croutine)
  (:use #:cl
        #:stumpwm)
  (:import-from #:stumpwm
                #:tile-group-current-frame
                #:split-frame
                #:window-frame
                #:frame-window
                #:frame-windows
                #:eval-command
                #:tile-group
                #:float-group
                #:float-window
                #:send-client-message)
  (:export #:remove-empty-frame
           #:echo-urgent-window
           #:map-nav-keys))


(in-package #:custom-routines)


;; Remove a frame when there is no window in it
(defun remove-empty-frame (win)
  (unless (typep win 'float-window)
    (let* ((f (window-frame win))
           (g (window-group win))
           (win-list (frame-windows g f)))
      (unless win-list
        (remove-split)))))


(defun echo-urgent-window (win)
  (message "^[^1^f1~A^] needs attention." (window-title win)))


(defun split-and-focus (group dir ratio)
  (let ((old-f (tile-group-current-frame group))
        (new-f (split-frame group dir ratio)))
    (if new-f
      (progn
        (when (frame-window old-f)
          (update-decoration (frame-window old-f)))
        (eval-command (format nil "fselect ~A" new-f)))
      (message "Cannot split smaller than minimum size."))))

(defcommand (hsplit-and-focus tile-group) (&optional (ratio "1/2")) (:string)
  "Hsplit a frame, and move focus to the new frame."
  (split-and-focus (current-group) :column (read-from-string ratio)))

(defcommand (vsplit-and-focus tile-group) (&optional (ratio "1/2")) (:string)
  "Vsplit a frame, and move focus to the new frame."
  (split-and-focus (current-group) :row (read-from-string ratio)))


(defcommand delete-maybe-remove (&optional (window (current-window))) ()
  "Delete a window. If invoked on an empty frame, remove that frame."
  (if window
    (send-client-message
      window :WM_PROTOCOLS
      (xlib:intern-atom *display* :WM_DELETE_WINDOW))
    (let ((g (current-group)))
      (unless (typep g 'float-group)
        (let* ((f (tile-group-current-frame g))
               (win-list (frame-windows g f)))
          (unless win-list
            (remove-split)))))))


(defun map-nav-keys (map layout)
  (case layout
    (:qwerty
      (define-key map (kbd "s-h") "prev-in-frame")
      (define-key map (kbd "s-l") "next-in-frame")
      (define-key map (kbd "s-k") "rc-prev-frame-or-window")
      (define-key map (kbd "s-j") "rc-next-frame-or-window")

      (define-key map (kbd "s-H") "move-window left")
      (define-key map (kbd "s-L") "move-window right")
      (define-key map (kbd "s-K") "move-window up")
      (define-key map (kbd "s-J") "move-window down")

      (define-key map (kbd "s-s") "iresize"))

    (:colemak-dh
      (define-key map (kbd "s-k") "prev-in-frame")
      (define-key map (kbd "s-i") "next-in-frame")
      (define-key map (kbd "s-e") "rc-prev-frame-or-window")
      (define-key map (kbd "s-n") "rc-next-frame-or-window")

      (define-key map (kbd "s-K") "move-window left")
      (define-key map (kbd "s-I") "move-window right")
      (define-key map (kbd "s-E") "move-window up")
      (define-key map (kbd "s-N") "move-window down")

      (define-key map (kbd "s-s") "colemak-dh-iresize"))))


(defcommand switch-kb-layout (&optional (layout nil)) (:string)
  "Switch keyboard layout and map navigation keys accordingly."
  (cond
    ((equal layout nil)
     (setf cglobal:*keyboard-layout* (if (eql cglobal:*keyboard-layout* :qwerty)
                                        :colemak-dh
                                        :qwerty))
     (map-nav-keys stumpwm::*top-map* cglobal:*keyboard-layout*)
     (message "Switched to keyboard layout: ^[^2^f1~A^]" cglobal:*keyboard-layout*))
    (t
     (setf cglobal:*keyboard-layout* (intern (string-upcase layout) "KEYWORD"))
     (map-nav-keys stumpwm::*top-map* cglobal:*keyboard-layout*)
     (message "Switched to keyboard layout: ^[^2^f1~A^]" cglobal:*keyboard-layout*))))

