(in-package #:stumpwm-user)

(import `(stumpwm::window-frame
          stumpwm::frame-window
          stumpwm::frame-windows
          stumpwm::tile-group-current-frame
          stumpwm::split-frame
          stumpwm::focus-prev-frame
          stumpwm::frame-number
          stumpwm::group-frames
          stumpwm::tile-group
          stumpwm::sort-windows
          stumpwm::head-mode-line
          stumpwm::eval-command
          stumpwm::send-client-message
          stumpwm.floating-group:float-group
          stumpwm.floating-group::float-window
          stumpwm.floating-group::*float-window-border*
          stumpwm.floating-group::*float-window-title-height*))


;;--------- Global Variables ---------

(defparameter *rc-group-count* 9)
(defparameter *rc-modules* `("battery-portable"
                             "cpu"
                             "mem"
                             "amixer"
                             "stumptray"
                             "ttf-fonts"))
(defparameter *rc-local-modules* `("useless-gaps"
                                   "group-set"))
; ~/.stumpwm.d/local-modules/
(defparameter *rc-local-modules-dir*
  (let* ((rel-modules-dir (make-pathname :directory '(:relative ".stumpwm.d" "local-modules"))))
    (merge-pathnames rel-modules-dir (user-homedir-pathname))))


;;--------- StumpWM Variables ---------

(setf *startup-message* nil)

(setf *window-border-style* :tight)
(setf *normal-border-width* 1)
(setf *transient-border-width* 1)
(setf *maxsize-border-width* 1)

(setf *mouse-focus-policy* :click)

(setf *float-window-border* 1)
(setf *float-window-title-height* 1)


;;--------- Modules ---------

(let* ((full-module-paths (mapcar #'(lambda (p)
                                      (merge-pathnames
                                        (make-pathname :directory `(:relative ,p))
                                        *rc-local-modules-dir*))
                                  *rc-local-modules*)))
  (mapcar #'add-to-load-path full-module-paths))

(mapcar #'load-module (append *rc-modules* *rc-local-modules*))


;;--------- Module Variables ---------

(setf useless-gaps:*useless-gaps-size* 4)


;;--------- Custom Functions and Commands ---------

(defun rc-split-and-focus (group dir ratio)
  (let ((old-f (tile-group-current-frame group))
        (new-f (split-frame group dir ratio)))
    (if new-f
      (progn
        (when (frame-window old-f)
          (update-decoration (frame-window old-f)))
        (eval-command (format nil "fselect ~A" new-f)))
      (message "Cannot split smaller than minimum size."))))


(defcommand rc-delete-maybe-remove (&optional (window (current-window))) ()
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

(defcommand (rc-fprev tile-group) () ()
  "Switch to the previous frame."
  (focus-prev-frame (current-group)))

(defcommand (rc-hsplit-and-focus tile-group) (&optional (ratio "1/2")) (:string)
  "Hsplit a frame, and move focus to the new frame."
  (rc-split-and-focus (current-group) :column (read-from-string ratio)))

(defcommand (rc-vsplit-and-focus tile-group) (&optional (ratio "1/2")) (:string)
  "Vsplit a frame, and move focus to the new frame."
  (rc-split-and-focus (current-group) :row (read-from-string ratio)))

(defcommand (rc-next-float-window float-group) () ()
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

(defcommand (rc-prev-float-window float-group) () ()
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

(defcommand rc-next-frame-or-window () ()
  "Switch to the next frame or window depending on the type of the current group."
  (if (typep (current-group) 'float-group)
    (rc-next-float-window)
    (fnext)))

(defcommand rc-prev-frame-or-window () ()
  "Switch to the previous frame or window depending on the type of the current group."
  (if (typep (current-group) 'float-group)
    (rc-prev-float-window)
    (rc-fprev)))

(defcommand rc-switch-group-in-group-set () ()
  "Switch to the other group in a group set."
  (let* ((gset (gset:current-group-set))
         (cur-group-nr (and gset (gset:gset-current-group-nr gset))))
    (case cur-group-nr
      (0 (gset:switch-to-group-set gset 1))
      (1 (gset:switch-to-group-set gset 0))
      ((nil) (message "Group '~A' does not belong to any group set"
                      (group-name (current-group)))))))

(defcommand rc-move-window-to-group-set (to-group-set) (:string)
  "Move a window to another group set."
  (let ((window (current-window)))
    (when window
      (let ((gset (gset:find-group-set (current-screen) to-group-set)))
        (if gset
          (gset:move-window-to-group-set window gset)
          (message "Group set '~A' not found" to-group-set))))))

(defcommand rc-move-all-windows-to-other-group () ()
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
    (when (typep cur-group 'tile-group)
      (let ((frames (group-frames cur-group)))
        (dolist (f frames)
          (unless (= 0 (frame-number f))
            (remove-split cur-group f)))))
    (gset:switch-to-group-set gset other-group-nr)))

(defcommand rc-start-swank (&optional port) (:string)
  "Start the SWANK server."
  (swank:create-server :port (parse-integer (or port "4005"))
                       :dont-close t))

(defcommand rc-stop-swank (&optional port) (:string)
  "Stop the SWANK server."
  (swank:stop-server (parse-integer (or port "4005"))))


;;--------- Hooks ---------

;; Remove a frame when there is no window in it
(defun rc-remove-empty-frame (win)
  (unless (typep win 'float-window)
    (let* ((f (window-frame win))
           (g (window-group win))
           (win-list (frame-windows g f)))
      (unless win-list
        (remove-split)))))

(add-hook *destroy-window-hook* 'rc-remove-empty-frame)


;;--------- Key Bindings ---------

(set-prefix-key (kbd "s-t"))

(define-key *top-map* (kbd "s-RET") "exec xterm")

(define-key *top-map* (kbd "s-C") "rc-delete-maybe-remove")
(define-key *top-map* (kbd "s-h") "prev-in-frame")
(define-key *top-map* (kbd "s-l") "next-in-frame")
(define-key *top-map* (kbd "s-'") "windowlist")

(define-key *top-map* (kbd "s-k") "rc-prev-frame-or-window")
(define-key *top-map* (kbd "s-j") "rc-next-frame-or-window")

(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-J") "move-window down")

(define-key *top-map* (kbd "s-i") "rc-hsplit-and-focus")
(define-key *top-map* (kbd "s--") "rc-vsplit-and-focus")
(define-key *top-map* (kbd "s-=") "balance-frames")
(define-key *top-map* (kbd "s-s") "iresize")

(define-key *top-map* (kbd "s-f") "fullscreen")

(define-key *top-map* (kbd "s-r") "exec")
(define-key *top-map* (kbd "s-:") "eval")
(define-key *top-map* (kbd "s-;") "colon")

(loop for gs from 1 to *rc-group-count*
      for key = (format nil "s-~A" gs)
      for cmd = (format nil "gset-select ~A" gs)
      do (define-key *top-map* (kbd key) cmd))
(define-key *top-map* (kbd "s-SPC") "rc-switch-group-in-group-set")
(loop for gs from 1 to *rc-group-count*
      for char in '(#\! #\@ #\# #\$ #\% #\^ #\& #\* #\()
      for key = (format nil "s-~A" char)
      for cmd = (format nil "rc-move-window-to-group-set ~A" gs)
      do (define-key *top-map* (kbd key) cmd))
(define-key *top-map* (kbd "S-s-SPC") "rc-move-all-windows-to-other-group")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-1+")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle")


;;--------- Groups ---------

(loop for gs from 1 to *rc-group-count*
      for gs-name = (format nil "~A" gs)
      do (gset:add-group-set (current-screen) gs-name
                             `(("T" tile-group)
                               ("F" float-group))))
(gset:gset-select "1")


;;--------- Appearance ---------

(set-focus-color "#535d6c")
(set-unfocus-color "#000000")
(set-float-focus-color "#535d6c")
(set-float-unfocus-color "#000000")

(setf xft:*font-dirs* `("/usr/share/fonts/dejavu"))
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 10))


;;--------- Mode Line ---------

(setf *screen-mode-line-format* `("^[^7^R %n ^r^] %d ^[^7❱^] %c %t ^[^7❱^] %M ^[^7❱^] BAT: %B "))
(setf *mode-line-position* :top)
(setf *mode-line-timeout* 1)

(unless (head-mode-line (current-head))
  (toggle-mode-line (current-screen) (current-head)))

(defvar *rc-stumptray-enabled* nil)
(unless *rc-stumptray-enabled*
  (setf *rc-stumptray-enabled* t)
  (stumptray:stumptray))


;;--------- Daemons ---------

(run-shell-command "feh --bg-scale ~/.dot-files/awesome/themes/default/background.jpg")

(run-shell-command  "ibus-daemon -d -x -r -n stumpwm")
(run-shell-command  "xautolock -time 10 -corners '00+-' -locker slock")
(let* ((compton-path (make-pathname :directory '(:relative "app_inst" "compton" "bin")
                                    :name "compton"))
       (compton-abs-path (merge-pathnames compton-path (user-homedir-pathname)))
       (compton-cmd (concatenate 'string
                                 (namestring compton-abs-path)
                                 " -c -t-4 -l-4 -r4 -o.75 -f -D7 -I.07 -O.07 --opacity-rule '90:class_g*?=\"xterm\"' --opacity-rule '75:window_type=\"dock\"'")))
  (run-shell-command compton-cmd))
