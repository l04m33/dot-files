(in-package #:stumpwm-user)

(import `(stumpwm::window-urgent-p
          stumpwm::frame-windows
          stumpwm::tile-group-current-frame
          stumpwm::focus-prev-frame
          stumpwm::frame-number
          stumpwm::group-frames
          stumpwm::tile-group
          stumpwm::sort-windows
          stumpwm::head-mode-line
          stumpwm::eval-command
          stumpwm::float-group
          stumpwm::float-window
          stumpwm::unfloat-window
          stumpwm::*float-window-border*
          stumpwm::*float-window-title-height*
          stumpwm::setup-iresize
          stumpwm::resize-unhide
          stumpwm::abort-resize-p))


;;--------- Debugging ---------

(defparameter *rc-log-file* (data-dir-file "debug" "log"))
(setf *debug-level* 0)
(ignore-errors (delete-file *rc-log-file*))
(redirect-all-output *rc-log-file*)


;;--------- Global Variables ---------

(setf *random-state* (make-random-state t))


;;--------- StumpWM Behaviors ---------

(setf *startup-message* nil)
(setf *mouse-focus-policy* :click)
(setf *input-history-ignore-duplicates* t)
(setf *maximum-completions* 20)


;;--------- Modules ---------

(defparameter *rc-modules-common* '(;;"stumptray"
                                    "ttf-fonts"
                                    "swm-gaps"))
(defparameter *rc-modules-linux* '("battery-portable"
                                   "cpu"
                                   "mem"
                                   "amixer"))
(defparameter *rc-modules*
  (if (string= "Linux" (software-type))
    (nconc (copy-list *rc-modules-common*) *rc-modules-linux*)
    *rc-modules-common*))
(defparameter *rc-local-modules* `("custom-utils"
                                   "custom-globals"
                                   "group-set"
                                   "custom-routines"
                                   "stumpwm-patches"))

; ~/.stumpwm.d/local-modules/
(defparameter *rc-local-modules-dir*
  (merge-pathnames
    (make-pathname
      :directory (append '(:relative) '("local-modules")))
    *data-dir*))

(let* ((full-module-paths (mapcar #'(lambda (p)
                                      (merge-pathnames
                                        (make-pathname :directory `(:relative ,p))
                                        *rc-local-modules-dir*))
                                  *rc-local-modules*)))
  (mapcar #'add-to-load-path full-module-paths))

(mapcar #'load-module (append *rc-modules* *rc-local-modules*))


;;--------- Module Variables ---------

(setf swm-gaps:*inner-gaps-size* 5)
(setf swm-gaps:*outer-gaps-size* 10)
(setf swm-gaps:*head-gaps-size* 0)
(when (not swm-gaps:*gaps-on*)
  (eval-command "toggle-gaps"))

(setf cglobal:*keyboard-layout* :colemak-dh)


;;--------- Custom Functions and Commands ---------

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
    (fprev)))

(defcommand rc-switch-group-in-group-set () ()
  "Switch to the other group in a group set."
  (let* ((gset (gset:current-group-set))
         (cur-group-nr (and gset (gset:gset-current-group-nr gset))))
    (case cur-group-nr
      (0 (gset:switch-to-group-set gset 1))
      (1 (gset:switch-to-group-set gset 0))
      ((nil) (message "Group ^[^2~A^] does not belong to any group set."
                      (group-name (current-group)))))))

(defcommand rc-move-window-to-group-set (to-group-set) (:string)
  "Move a window to another group set."
  (let ((window (current-window)))
    (when window
      (let ((gset (gset:find-group-set (current-screen) to-group-set)))
        (if gset
          (gset:move-window-to-group-set window gset)
          (message "Group set ^[^2~A^] not found." to-group-set))))))

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

(defcommand rc-show-group-overview () ()
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

(defun rc-get-random-wp (&optional dir exclude)
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

(defcommand rc-random-wp (&optional dir) (:string)
  "Switch to a wallpaper randomly selected from DIR."
  (let ((wp (rc-get-random-wp (if dir
                                (pathname dir)
                                nil)
                              cglobal:*current-wp*)))
    (when wp
      (message "Setting wallpaper: ^[^2^f1~A^]" wp)
      (setf cglobal:*current-wp* wp)
      (run-shell-command (format nil "feh --bg-fill ~a" wp)))))

(defcommand rc-start-swank (&optional port) (:string)
  "Start the SWANK server."
  (swank:create-server :port (parse-integer (or port "4005"))
                       :dont-close t))

(defcommand rc-stop-swank (&optional port) (:string)
  "Stop the SWANK server."
  (swank:stop-server (parse-integer (or port "4005"))))

(defcommand rc-start-vlime (&optional port) (:string)
  "Start the Vlime server."
  (vlime:main :port (parse-integer (or port "7002")) :backend :vlime-usocket))

;;--------- Hooks ---------

(add-hook *destroy-window-hook* 'croutine:remove-empty-frame)
(add-hook *urgent-window-hook* 'croutine:echo-urgent-window)


;;--------- Key Bindings ---------

(set-prefix-key (kbd "s-t"))

(croutine:map-nav-keys *top-map* cglobal:*keyboard-layout*)

(define-key *top-map* (kbd "s-RET") "exec xterm")

(define-key *top-map* (kbd "s-C") "delete-maybe-remove")
(define-key *top-map* (kbd "s-/") "windowlist")

(define-key *top-map* (kbd "s-,") "hsplit-and-focus")
(define-key *top-map* (kbd "s-.") "vsplit-and-focus")
(define-key *top-map* (kbd "s-=") "balance-frames")

(define-key *top-map* (kbd "s-f") "fullscreen")

(define-key *top-map* (kbd "s-r") "exec")
(define-key *top-map* (kbd "s-:") "eval")
(define-key *top-map* (kbd "s-;") "colon")

(loop for gs from cglobal:*first-group* to cglobal:*last-group*
      for key = (format nil "s-~A" gs)
      for cmd = (format nil "gset-select ~A" gs)
      do (define-key *top-map* (kbd key) cmd))
(define-key *top-map* (kbd "s-SPC") "rc-switch-group-in-group-set")
(loop for gs from cglobal:*first-group* to cglobal:*last-group*
      for char in '(#\) #\! #\@ #\# #\$ #\% #\^ #\& #\* #\()
      for key = (format nil "s-~A" char)
      for cmd = (format nil "rc-move-window-to-group-set ~A" gs)
      do (define-key *top-map* (kbd key) cmd))
(define-key *top-map* (kbd "S-s-SPC") "rc-move-all-windows-to-other-group")
(define-key *top-map* (kbd "s-p") "rc-show-group-overview")
(define-key *top-map* (kbd "s-P") "vgroups")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-1+")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle")

(define-interactive-keymap (colemak-dh-iresize tile-group) (:on-enter #'setup-iresize
                                                            :on-exit #'resize-unhide
                                                            :abort-if #'abort-resize-p)
  ((kbd "Up") "resize-direction up")
  ((kbd "e") "resize-direction up")

  ((kbd "Down") "resize-direction down")
  ((kbd "n") "resize-direction down")

  ((kbd "Left") "resize-direction left")
  ((kbd "k") "resize-direction left")

  ((kbd "Right") "resize-direction right")
  ((kbd "i") "resize-direction right"))


;;--------- Groups ---------

(loop for gs from cglobal:*first-group* to cglobal:*last-group*
      for gs-name = (format nil "~A" gs)
      do (gset:add-group-set (current-screen) gs-name
                             `(("T" tile-group)
                               ("F" float-group))))
(gset:gset-select "1")


;;--------- Appearance ---------

(setf *colors* `("#1d1f21" ; black   (background)
                 "#cc6666" ; red
                 "#b5bd68" ; green
                 "#f0c674" ; yellow
                 "#81a2be" ; blue
                 "#b294bb" ; magenta (purple)
                 "#8abeb7" ; cyan    (aqua)
                 "#c5c8c6" ; white   (foreground)

                 "#969896" ; gray    (comment)
                 "#282a2e" ; (current line)
                 "#373b41" ; (selection)
                 "#de935f")) ; (orange)

(set-fg-color (nth 7 *colors*))
(set-bg-color (nth 0 *colors*))
(dolist (screen *screen-list*)
  (update-color-map screen))

(setf *window-border-style* :tight)
(setf *normal-border-width* 1)
(setf *transient-border-width* 1)
(setf *maxsize-border-width* 1)

(setf *float-window-border* 1)
(setf *float-window-title-height* 1)

(set-msg-border-width 0)
(set-frame-outline-width 2)

(set-focus-color (nth 4 *colors*))
(set-unfocus-color (nth 0 *colors*))
(set-float-focus-color (nth 4 *colors*))
(set-float-unfocus-color (nth 0 *colors*))

;; Tell StumpWM where to find the fonts
(setf xft:*font-dirs* `(,cglobal:*fonts-dir*))
(xft:cache-fonts)
(run-shell-command "xset fp+ \"${HOME}/.local/share/fonts/tamzen-font-bdf\"" t)
(run-shell-command "xset fp rehash" t)

(set-font (list
            "-*-tamzenforpowerline-medium-*-*-*-15-*-*-*-*-*-*-*"
            (make-instance 'xft:font :family "WenQuanYi Zen Hei Mono" :subfamily "Regular" :size 11)))


;;--------- Mode Line ---------

(if (string= "Linux" (software-type))
  (progn
    (setf *screen-mode-line-format* `("^[^(:fg \"#1d1f21\")^(:bg \"#81a2be\") %n ^]"
                                      "^[^(:fg \"#81a2be\")^(:bg \"#8abeb7\")^]"
                                      "^[^(:fg \"#1d1f21\")^(:bg \"#8abeb7\") %d ^]"
                                      "^[^(:fg \"#8abeb7\")^(:bg \"#b5bd68\")^]"
                                      "^[^(:fg \"#1d1f21\")^(:bg \"#b5bd68\") %C ^]"
                                      "^[^(:fg \"#b5bd68\")^(:bg \"#f0c674\")^]"
                                      "^[^(:fg \"#1d1f21\")^(:bg \"#f0c674\") %M ^]"
                                      "^[^(:fg \"#f0c674\")^(:bg \"#de935f\")^]"
                                      "^[^(:fg \"#1d1f21\")^(:bg \"#de935f\") BAT: %B ^]"
                                      "^[^(:fg \"#de935f\")^(:bg \"#969896\")^]"
                                      "^[^(:fg \"#969896\")^(:bg \"#373b41\")^]"
                                      "^[^(:fg \"#373b41\")^]"
                                      "^>"
                                      "^[^(:fg \"#373b41\")^]"
                                      "^[^(:fg \"#969896\")^(:bg \"#373b41\")^]"
                                      "^[^(:fg \"#1d1f21\")^(:bg \"#969896\")  ^]"))
    ;; The CPU package may not be available
    (setf (symbol-value (find-symbol "*CPU-MODELINE-FMT*" "CPU")) "%c %t"))
  (setf *screen-mode-line-format* `("^[^(:fg \"#1d1f21\")^(:bg \"#81a2be\") %n ^]"
                                    "^[^(:fg \"#81a2be\")^(:bg \"#8abeb7\")^]"
                                    "^[^(:fg \"#1d1f21\")^(:bg \"#8abeb7\") %d ^]"
                                    "^[^(:fg \"#8abeb7\")^(:bg \"#969896\")^]"
                                    "^[^(:fg \"#969896\")^(:bg \"#373b41\")^]"
                                    "^[^(:fg \"#373b41\")^]"
                                    "^>"
                                    "^[^(:fg \"#373b41\")^]"
                                    "^[^(:fg \"#969896\")^(:bg \"#373b41\")^]"
                                    "^[^(:fg \"#1d1f21\")^(:bg \"#969896\")  ^]")))
(setf *mode-line-position* :top)
(setf *mode-line-timeout* 1)
(setf *mode-line-foreground-color* (nth 8 *colors*))
(setf *mode-line-background-color* (nth 0 *colors*))
(setf *mode-line-border-width* 0)

(unless (head-mode-line (current-head))
  (toggle-mode-line (current-screen) (current-head)))

;;(defvar *rc-stumptray-enabled* nil)
;;(unless *rc-stumptray-enabled*
;;  (setf *rc-stumptray-enabled* t)
;;  (stumptray:stumptray))


;;--------- Daemons ---------

(eval-command "rc-random-wp")
(run-shell-command  "ibus-daemon -d -x -r -n stumpwm")
(run-shell-command  "xautolock -time 10 -corners '00+-' -locker slock")
(run-shell-command  "compton -c -t-4 -l-4 -r4 -o.75 -f -D7 -I.07 -O.07 --opacity-rule '90:class_g*?=\"xterm\"' --opacity-rule '75:window_type=\"dock\"'")

