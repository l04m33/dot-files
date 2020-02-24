(defpackage #:custom-routines
  (:nicknames #:croutine)
  (:use #:cl
        #:stumpwm)
  (:import-from #:stumpwm
                #:frame-number
                #:group-frames
                #:tile-group-current-frame
                #:split-frame
                #:window-frame
                #:frame-window
                #:frame-windows
                #:eval-command
                #:tile-group
                #:float-group
                #:float-window
                #:unfloat-window
                #:sort-windows
                #:window-urgent-p
                #:send-client-message)
  (:export #:remove-empty-frame
           #:echo-urgent-window))


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

