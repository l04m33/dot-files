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
                #:sort-windows
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
      (define-key map (kbd "s-k") "prev-frame-or-window")
      (define-key map (kbd "s-j") "next-frame-or-window")

      (define-key map (kbd "s-H") "move-window left")
      (define-key map (kbd "s-L") "move-window right")
      (define-key map (kbd "s-K") "move-window up")
      (define-key map (kbd "s-J") "move-window down")

      (define-key map (kbd "s-s") "iresize"))

    (:colemak-dh
      (define-key map (kbd "s-k") "prev-in-frame")
      (define-key map (kbd "s-i") "next-in-frame")
      (define-key map (kbd "s-e") "prev-frame-or-window")
      (define-key map (kbd "s-n") "next-frame-or-window")

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


(defcommand (next-float-window float-group) () ()
  "Switch to the next float window."
  (let ((window (current-window)))
    (when window
      (let* ((group (current-group))
             (group-windows (sort-windows group))
             (next-window
               (loop for w from 0 to (1- (length group-windows))
                     when (= (window-number window)
                             (window-number (nth w group-windows)))
                     return (or (nth (1+ w) group-windows)
                                (nth 0 group-windows)))))
        (when next-window
          (focus-window next-window t))))))

(defcommand (prev-float-window float-group) () ()
  "Switch to the previous float window."
  (let ((window (current-window)))
    (when window
      (let* ((group (current-group))
             (group-windows (sort-windows group))
             (prev-window
               (loop for w from (1- (length group-windows)) downto 0
                     when (= (window-number window)
                             (window-number (nth w group-windows)))
                     return (if (> w 0)
                              (nth (1- w) group-windows)
                              (car (last group-windows))))))
        (when prev-window
          (focus-window prev-window t))))))

(defcommand next-frame-or-window () ()
  "Switch to the next frame or window depending on the type of the current group."
  (if (typep (current-group) 'float-group)
    (next-float-window)
    (fnext)))

(defcommand prev-frame-or-window () ()
  "Switch to the previous frame or window depending on the type of the current group."
  (if (typep (current-group) 'float-group)
    (prev-float-window)
    (fprev)))


(defun get-random-wp (&optional dir exclude)
  "Get a random wallpaper from DIR."
  (let* ((wp-dir (or dir cglobal:*wp-dir*))
         (wp-wild (merge-pathnames wp-dir (make-pathname :name :wild :type :wild)))
         (wp-list (remove-if
                    #'(lambda (p)
                        (or (null (pathname-name p))
                            (and (not (null exclude))
                                 (pathname-match-p p exclude))))
                    (directory wp-wild)))
         (wp-count (length wp-list)))
      (if (> wp-count 0)
        (elt wp-list (random wp-count))
        nil)))

(defcommand random-wp (&optional dir) (:string)
  "Switch to a wallpaper randomly selected from DIR."
  (let ((wp (get-random-wp (if dir
                             (pathname dir)
                             nil)
                           cglobal:*current-wp*)))
    (when wp
      (message "Setting wallpaper: ^[^2^f1~A^]" wp)
      (setf cglobal:*current-wp* wp)
      (run-shell-command (format nil "feh --bg-fill ~a" wp)))))

(defcommand start-swank (&optional port) (:string)
  "Start the SWANK server."
  (swank:create-server :port (parse-integer (or port "4005"))
                       :dont-close t))

(defcommand stop-swank (&optional port) (:string)
  "Stop the SWANK server."
  (swank:stop-server (parse-integer (or port "4005"))))

(defcommand start-vlime (&optional port) (:string)
  "Start the Vlime server."
  (vlime:main :port (parse-integer (or port "7002")) :backend :vlime-usocket))

