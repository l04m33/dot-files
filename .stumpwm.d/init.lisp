(in-package #:stumpwm)


;;--------- Global Variables ---------

(defparameter *rc-group-count* 9)
(defparameter *rc-modules* `("battery-portable"
                             "cpu"
                             "mem"
                             "amixer"
                             "stumptray"
                             "ttf-fonts"))
(defparameter *rc-local-modules* `("useless-gaps"))
; ~/.stumpwm.d/local-modules/
(defparameter *rc-local-modules-dir*
  (let* ((rel-modules-dir (make-pathname :directory '(:relative ".stumpwm.d" "local-modules"))))
    (merge-pathnames rel-modules-dir (user-homedir-pathname))))


;;--------- StumpWM Variables ---------

(setf *window-border-style* :tight)
(setf *normal-border-width* 1)
(setf *transient-border-width* 1)
(setf *maxsize-border-width* 1)


;;--------- Appearance ---------

(set-focus-color "#535d6c")
(set-unfocus-color "#000000")
(set-float-focus-color "#535d6c")
(set-float-unfocus-color "#000000")

(setf xft:*font-dirs* `("/usr/share/fonts/dejavu"))
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 10))


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

(defun split-and-focus (group dir ratio)
  (let ((old-f (tile-group-current-frame group))
        (new-f (split-frame group dir ratio)))
    (if new-f
      (progn
        (when (frame-window old-f)
          (update-decoration (frame-window old-f)))
        (eval-command (format nil "fselect ~A" new-f)))
      (message "Cannot split smaller than minimum size."))))


(defcommand (delete-maybe-remove tile-group) (&optional (window (current-window))) ()
  (if window
    (let* ((f (window-frame window))
           (g (window-group window))
           (win-list (frame-windows g f)))
      (unless (> (length win-list) 1)
        (remove-split))
      (send-client-message window :WM_PROTOCOLS (xlib:intern-atom *display* :WM_DELETE_WINDOW)))
    (let* ((g (current-group))
           (f (tile-group-current-frame g))
           (win-list (frame-windows g f)))
      (unless (> (length win-list) 0)
        (remove-split)))))

(defcommand (fprev tile-group) () ()
  (focus-prev-frame (current-group)))

(defcommand (hsplit-and-focus tile-group) (&optional (ratio "1/2")) (:string)
  (split-and-focus (current-group) :column (read-from-string ratio)))

(defcommand (vsplit-and-focus tile-group) (&optional (ratio "1/2")) (:string)
  (split-and-focus (current-group) :row (read-from-string ratio)))


;;--------- Key Bindings ---------

(set-prefix-key (kbd "s-t"))

(define-key *top-map* (kbd "s-RET") "exec xterm")

(define-key *top-map* (kbd "s-C") "delete-maybe-remove")
(define-key *top-map* (kbd "s-h") "prev-in-frame")
(define-key *top-map* (kbd "s-l") "next-in-frame")
(define-key *top-map* (kbd "s-'") "windowlist")

(define-key *top-map* (kbd "s-k") "fprev")
(define-key *top-map* (kbd "s-j") "fnext")

(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-J") "move-window down")

(define-key *top-map* (kbd "s-i") "hsplit-and-focus")
(define-key *top-map* (kbd "s--") "vsplit-and-focus")
(define-key *top-map* (kbd "s-=") "balance-frames")
(define-key *top-map* (kbd "s-s") "iresize")

(define-key *top-map* (kbd "s-f") "fullscreen")

(define-key *top-map* (kbd "s-r") "exec")
(define-key *top-map* (kbd "s-:") "eval")
(define-key *top-map* (kbd "s-;") "colon")

(loop for g from 1 to *rc-group-count*
      for key = (format nil "s-~A" g)
      for cmd = (format nil "gselect ~A" g)
      do (define-key *top-map* (kbd key) cmd))

(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-1+")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle")


;;--------- Groups ---------

(grename "1")
(loop for g from 2 to *rc-group-count*
      for g-name = (format nil "~A" g)
      do (add-group (current-screen) g-name :background t))


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
