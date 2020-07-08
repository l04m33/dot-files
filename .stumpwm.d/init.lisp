(in-package #:stumpwm-user)

(import `(stumpwm::tile-group
          stumpwm::float-group
          stumpwm::head-mode-line
          stumpwm::eval-command
          stumpwm::*float-window-border*
          stumpwm::*float-window-title-height*))


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

(defparameter *rc-modules-common* (append (if (find-package "XFT")
                                            '("ttf-fonts")
                                            '())
                                          '(;;"stumptray"
                                            "swm-gaps")))
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
                                   "custom-keymaps"
                                   "stumpwm-patches"))

; ~/.stumpwm.d/stumpwm-contrib/
(defparameter *rc-modules-dir*
  (merge-pathnames
    (make-pathname
      :directory (append '(:relative) '("stumpwm-contrib")))
    *data-dir*))

(init-load-path *rc-modules-dir*)

; ~/.stumpwm.d/local-modules/
(defparameter *rc-local-modules-dir*
  (merge-pathnames
    (make-pathname
      :directory (append '(:relative) '("local-modules")))
    *data-dir*))

(let* ((full-local-module-paths (mapcar #'(lambda (p)
                                            (merge-pathnames
                                              (make-pathname :directory `(:relative ,p))
                                              *rc-local-modules-dir*))
                                        *rc-local-modules*)))
  (mapcar #'add-to-load-path full-local-module-paths))

(mapcar #'load-module (append *rc-modules* *rc-local-modules*))


;;--------- Module Behaviors ---------

(setf swm-gaps:*inner-gaps-size* 5)
(setf swm-gaps:*outer-gaps-size* 10)
(setf swm-gaps:*head-gaps-size* 0)
(when (not swm-gaps:*gaps-on*)
  (eval-command "toggle-gaps"))

(loop for gs from cglobal:*first-group* to cglobal:*last-group*
      for gs-name = (format nil "~A" gs)
      do (gset:add-group-set (current-screen) gs-name
                             `(("T" tile-group)
                               ("F" float-group))))
(gset:gset-select "1")


;;--------- Hooks ---------

(add-hook *destroy-window-hook* 'croutine:remove-empty-frame)
(add-hook *urgent-window-hook* 'croutine:echo-urgent-window)


;;--------- Key Bindings ---------

(set-prefix-key (kbd "s-t"))
(eval-command "top-map-colemak-dh")


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

(croutine:cache-fonts)
(if (find-package "XFT")
  (set-font (list
              "-*-tamzenforpowerline-medium-*-*-*-15-*-*-*-*-*-*-*"
              (make-instance (find-symbol "FONT" "XFT")
                             :family "WenQuanYi Zen Hei Mono"
                             :subfamily "Regular"
                             :size 11)))
  (set-font "-*-tamzenforpowerline-medium-*-*-*-15-*-*-*-*-*-*-*"))


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

(eval-command "random-wp")
(run-shell-command  "ibus-daemon -d -x -r -n stumpwm")
(run-shell-command  "xautolock -time 10 -corners '00+-' -locker slock")
(run-shell-command  "compton -c -t-4 -l-4 -r4 -o.75 -f -D7 -I.07 -O.07 --opacity-rule '90:class_g*?=\"xterm\"' --opacity-rule '75:window_type=\"dock\"'")
(run-shell-command  "xrdb -merge ~/.Xresources") ;; Just to be sure

