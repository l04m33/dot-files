(defpackage #:custom-routines
  (:nicknames #:croutine)
  (:use #:cl
        #:stumpwm)
  (:import-from #:stumpwm
                #:frame
                #:frame-head
                #:frame-number
                #:group-frames
                #:tile-group-current-frame
                #:tile-group-frame-tree
                #:clear-frame-outlines
                #:draw-frame-outlines
                #:resize-frame
                #:split-frame
                #:window-frame
                #:frame-window
                #:frame-windows
                #:neighbour
                #:pull-window
                #:eval-command
                #:tile-group
                #:tile-window
                #:float-group
                #:float-window
                #:unfloat-window
                #:float-window-move-resize
                #:sort-windows
                #:window-urgent-p
                #:send-client-message)
  (:export #:remove-empty-frame
           #:echo-urgent-window
           #:cache-fonts))


(in-package #:custom-routines)


;; Remove a frame when there is no window in it
(defun remove-empty-frame (win)
  (unless (typep win 'float-window)
    (let* ((f (window-frame win))
           (g (window-group win))
           (win-list (frame-windows g f)))
      (unless win-list
        (remove-split g f)))))


(defun echo-urgent-window (win)
  (if (find-package "XFT")
    (message "^[^1^f1~A^] needs attention." (window-title win))
    (message "^[^1^f0~A^] needs attention." (window-title win))))


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
            (remove-split g f)))))))


(defcommand next-float-window () ()
  "Switch to the next float window."
  (let ((window (current-window)))
    (when window
      (let* ((group (current-group))
             (group-windows (remove-if-not #'(lambda (w) (typep w 'float-window))
                                           (sort-windows group)))
             (next-window
               (loop for w from 0 to (1- (length group-windows))
                     when (= (window-number window)
                             (window-number (nth w group-windows)))
                     return (or (nth (1+ w) group-windows)
                                (nth 0 group-windows)))))
        (when next-window
          (focus-window next-window t))))))

(defcommand prev-float-window () ()
  "Switch to the previous float window."
  (let ((window (current-window)))
    (when window
      (let* ((group (current-group))
             (group-windows (remove-if-not #'(lambda (w) (typep w 'float-window))
                                           (sort-windows group)))
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
  (let ((cw (current-window)))
    (if (null cw)
      (when (typep (current-group) 'tile-group)
        (fnext))
      (if (typep cw 'float-window)
        (next-float-window)
        (fnext)))))

(defcommand prev-frame-or-window () ()
  "Switch to the previous frame or window depending on the type of the current group."
  (let ((cw (current-window)))
    (if (null cw)
      (when (typep (current-group) 'tile-group)
        (fprev))
      (if (typep cw 'float-window)
        (prev-float-window)
        (fprev)))))

(defcommand (float-this-maybe-remove tile-group) () ()
  (let* ((win (current-window))
         (frame (and win (typep win 'tile-window) (window-frame win)))
         (group (and win (window-group win)))
         (win-list (and frame (remove-if #'(lambda (w) (eq w win))
                                         (frame-windows group frame)))))
    (when win
      (float-window win group))
    (when (and frame (null win-list))
      (remove-split group frame)
      (focus-window win))))

(defcommand (float-or-unfloat-this tile-group) () ()
  (if (typep (current-window) 'float-window)
    (unfloat-this)
    (float-this-maybe-remove)))


(defun move-tile-window (win dir &optional (amount 1))
  (let* ((group (window-group win))
         (new-frame (window-frame win)))
    (dotimes (i amount)
      (setf new-frame (neighbour dir new-frame (group-frames group))))
    (when new-frame
      (pull-window win new-frame))))

(defun move-float-window (win dir &optional (amount 1))
  (let* ((x (window-x win))
         (y (window-y win))
         (new-x (case dir (:left (- x amount)) (:right (+ x amount)) (t x)))
         (new-y (case dir (:up (- y amount)) (:down (+ y amount)) (t y))))
    (float-window-move-resize win
                              :x new-x
                              :y new-y
                              :width (window-width win)
                              :height (window-height win))))

(defun win-op-step (&optional win)
  (let* ((head (if win
                 (window-head win)
                 (current-head)))
         (max-dim (max (frame-width head) (frame-height head))))
    (round (/ max-dim 40))))

(defcommand move-any-window (dir) ((:direction "Direction: "))
  (let ((win (current-window)))
    (when win
      (if (typep win 'tile-window)
        (move-tile-window win dir 1)
        (move-float-window win dir (win-op-step win))))))


(defun resize-tile-window (win-or-frame dir &optional (amount 1))
  (let* ((group (if (typep win-or-frame 'frame)
                  (current-group)  ;; XXX
                  (window-group win-or-frame)))
         (frame (if (typep win-or-frame 'frame)
                  win-or-frame
                  (window-frame win-or-frame)))
         (head (frame-head group frame)))
    (if (atom (tile-group-frame-tree group))
      (message "No more frames!")
      (progn
        (clear-frame-outlines group)
        (case dir
          (:up
           (resize-frame group frame (- amount) :height))
          (:down
           (resize-frame group frame amount :height))
          (:left
           (resize-frame group frame (- amount) :width))
          (:right
           (resize-frame group frame amount :width)))
        (draw-frame-outlines group head)))))

(defun resize-float-window (win dir &optional (amount 1))
  (let* ((width (window-width win))
         (height (window-height win))
         (new-width (case dir (:left (- width amount)) (:right (+ width amount)) (t width)))
         (new-height (case dir (:up (- height amount)) (:down (+ height amount)) (t height))))
    (float-window-move-resize win
                              :x (window-x win)
                              :y (window-y win)
                              :width new-width
                              :height new-height)))

(defcommand resize-any-window (dir) ((:direction "Direction: "))
  (let ((win (current-window)))
    (if win
      (if (typep win 'tile-window)
        (resize-tile-window win dir (win-op-step win))
        (resize-float-window win dir (win-op-step win)))
      (let ((group (current-group)))
        (when (typep group 'tile-group)
          (resize-tile-window (tile-group-current-frame group) dir (win-op-step)))))))


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
  (funcall (find-symbol "CREATE-SERVER" "SWANK")
           :port (parse-integer (or port "4005"))
           :dont-close t))

(defcommand stop-swank (&optional port) (:string)
  "Stop the SWANK server."
  (funcall (find-symbol "STOP-SERVER" "SWANK")
           (parse-integer (or port "4005"))))

(defcommand start-vlime (&optional port) (:string)
  "Start the Vlime server."
  (funcall (find-symbol "MAIN" "VLIME") :port (parse-integer (or port "7002")) :backend :vlime-usocket))


(defcommand switch-group-in-group-set () ()
  "Switch to the other group in a group set."
  (let* ((gset (gset:current-group-set))
         (cur-group-nr (and gset (gset:gset-current-group-nr gset))))
    (case cur-group-nr
      (0 (gset:switch-to-group-set gset 1))
      (1 (gset:switch-to-group-set gset 0))
      ((nil) (message "Group ^[^2~A^] does not belong to any group set."
                      (group-name (current-group)))))))

(defcommand move-window-to-group-set (to-group-set) (:string)
  "Move a window to another group set."
  (let ((window (current-window)))
    (when window
      (let ((gset (gset:find-group-set (current-screen) to-group-set)))
        (if gset
          (gset:move-window-to-group-set window gset)
          (message "Group set ^[^2~A^] not found." to-group-set))))))

(defcommand move-all-windows-to-other-group () ()
  "Move all windows in current group to the other group in a group set."
  (let* ((gset (gset:current-group-set))
         (gset-groups (gset:gset-groups gset))
         (cur-group-nr (gset:gset-current-group-nr gset))
         (other-group-nr (cond
                           ((= cur-group-nr 0) 1)
                           ((= cur-group-nr 1) 0)))
         (cur-group (elt gset-groups cur-group-nr))
         (other-group (elt gset-groups other-group-nr))
         (windows (group-windows cur-group)))
    (move-windows-to-group windows other-group)
    (if (typep cur-group 'tile-group)
      ;; tile-group -> float-group
      ;; Remove all tile-group frames since the tile-group is now empty.
      (let ((frames (group-frames cur-group)))
        (dolist (f frames)
          (unless (= 0 (frame-number f))
            (remove-split cur-group f))))
      ;; float-group -> tile-group
      (mapcar #'(lambda (w) (unfloat-window w other-group)) windows))
    (gset:switch-to-group-set gset other-group-nr)))

(defcommand show-group-overview () ()
  "Show brief stats of all groups in group sets."
  (labels
    ((write-group-stat (gset cur-group gnr stream)
       (let* ((group (nth gnr (gset:gset-groups gset)))
              (group-windows (group-windows group))
              (win-num (length group-windows)))
         (write-string " " stream)
         (if (string= (group-name group) (group-name cur-group))
           (write-string "*" stream)
           (if (> win-num 0)
             (let ((has-urgent-window
                     (loop for w in group-windows
                           when (window-urgent-p w) return t
                           finally (return nil))))
               (if has-urgent-window
                 (write-string "!" stream)
                 (if (> win-num 9)
                   (write-string "#" stream)
                   (format stream "~A" win-num))))
             (write-string "-" stream)))))
     (iter-groups (screen cur-group gnr stream)
       (loop for s from cglobal:*first-group* to cglobal:*last-group*
             for sname = (write-to-string s)
             do (write-group-stat (gset:find-group-set screen sname)
                                  cur-group gnr stream))))
    (let* ((screen (current-screen))
           (cur-group (current-group))
           (stat-message (with-output-to-string (out)
                           (iter-groups screen cur-group 0 out)
                           (format out " ~%")
                           (iter-groups screen cur-group 1 out)
                           (format out " ~%"))))
      (message "~A" stat-message))))


(defcommand (weighted-frames tile-group) (&optional (ratio "0.618")) (:string)
  (let ((group (current-group)))
    (if (<= (length (group-frames group)) 1)
      (message "There's only one frame!")
      (let* ((head (current-head group))
             (head-width (frame-width head))
             (head-height (frame-height head))

             (ratio-n (read-from-string ratio))

             (target-width (round (* head-width ratio-n)))
             (target-height (round (* head-height ratio-n)))

             (frame (tile-group-current-frame group))
             (width (frame-width frame))
             (height (frame-height frame))

             (delta-width (- target-width width))
             (delta-height (- target-height height)))
        (resize delta-width delta-height)))))


(defun cache-fonts ()
  "Tell StumpWM where to find the fonts"
  (setf (symbol-value (find-symbol "*FONT-DIRS*" "XFT")) `(,cglobal:*fonts-dir*))
  (funcall (find-symbol "CACHE-FONTS" "XFT"))
  (run-shell-command "xset fp+ \"${HOME}/.local/share/fonts/tamzen-font-bdf\"" t)
  (run-shell-command "xset fp rehash" t))

