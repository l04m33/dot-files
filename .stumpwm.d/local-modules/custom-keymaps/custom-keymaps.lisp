(defpackage #:custom-keymaps
  (:nicknames #:ckmap)
  (:use #:cl
        #:stumpwm)
  (:import-from #:stumpwm
                #:tile-group
                #:float-group
                #:enter-interactive-keymap
                #:exit-interactive-keymap
                #:setup-iresize
                #:resize-unhide
                #:abort-resize-p)
  (:export #:make-interactive-keymap
           #:tkbd))


(in-package #:custom-keymaps)


;; For keyboards/systems where the Super key is unavailable,
;; use Ctrl + Meta instead.
(defun tkbd (k-spec)
  (let ((k (kbd k-spec)))
    (if (and cglobal:*no-super-key* (stumpwm::key-super k))
      (progn
        (setf (stumpwm::key-super k)   nil
              (stumpwm::key-control k) t
              (stumpwm::key-meta k)    t)
        k)
      k)))


(defmacro make-interactive-keymap
    (name (&key on-enter on-exit abort-if (exit-on '((tkbd "RET")
                                                     (tkbd "ESC")
                                                     (tkbd "C-g"))))
     &body key-bindings)
  "Basically the same as define-interactive-keymap, but returns the keymap
so one can further customize it."
  (let* ((command (if (listp name) (car name) name))
         (exit-command (format nil "EXIT-~A" command))
         (keymap (gensym "m")))
    (multiple-value-bind (key-bindings decls docstring)
        (alexandria:parse-body key-bindings :documentation t)
      `(let ((,keymap (make-sparse-keymap)))
         ,@(loop for keyb in key-bindings
                 collect `(define-key ,keymap ,(first keyb)
                            ,(if (third keyb)
                                 (concatenate 'string "call-and-exit-kmap \""
                                              (second keyb) "\" " exit-command)
                                 (second keyb))))
         ,@(loop for keyb in exit-on
                 collect `(define-key ,keymap ,keyb ,exit-command))

         (defcommand ,name () ()
           ,@decls
           ,(or docstring
                (format nil "Starts interactive command \"~A\"" command))
           ,@(when abort-if `((when (funcall ,abort-if)
                                (return-from ,command))))

           ,@(when on-enter `((funcall ,on-enter)))
           (enter-interactive-keymap ,keymap (quote ,command)))

         (defcommand ,(intern exit-command) () ()
           ,@(when on-exit `((funcall ,on-exit)))
           (exit-interactive-keymap (quote ,command)))
         
         ,keymap))))


(defvar *common-top-maps*
  `((,(tkbd "s-RET") "exec xterm")
    (,(tkbd "s-S-RET") "exec kitty")

    (,(tkbd "s-C") "delete-maybe-remove")
    (,(tkbd "s-/") "windowlist")
    (,(tkbd "s-?") "frame-windowlist")

    (,(tkbd "s-m") "mark")
    (,(tkbd "s-M") "pull-marked")

    (,(tkbd "s-v") "toggle-always-on-top")
    (,(tkbd "s-V") "toggle-always-show")

    (,(tkbd "s-,") "hsplit-and-focus")
    (,(tkbd "s-.") "vsplit-and-focus")
    (,(tkbd "s-=") "balance-frames")
    (,(tkbd "s-+") "weighted-frames")

    (,(tkbd "s-f") "fullscreen")
    (,(tkbd "s-F") "float-or-unfloat-this")

    (,(tkbd "s-r") "exec")
    (,(tkbd "s-:") "eval")
    (,(tkbd "s-;") "colon")

    (,(tkbd "s-SPC")   "switch-group-in-group-set")
    (,(tkbd "S-s-SPC") "move-all-windows-to-other-group")

    (,(tkbd "s-p") "show-group-overview")
    (,(tkbd "s-P") "vgroups")
    
    (,(tkbd "XF86AudioLowerVolume") "amixer-Master-1-")
    (,(tkbd "XF86AudioRaiseVolume") "amixer-Master-1+")
    (,(tkbd "XF86AudioMute")        "amixer-Master-toggle")))


(defvar *group-set-maps*
  (append
    (loop for gs from cglobal:*first-group* to cglobal:*last-group*
          for key = (format nil "s-~A" gs)
          for cmd = (format nil "gset-select ~A" gs)
          collect (list (tkbd key) cmd))
    (loop for gs from cglobal:*first-group* to cglobal:*last-group*
          for char in '(#\) #\! #\@ #\# #\$ #\% #\^ #\& #\* #\()
          for key = (format nil "s-~A" char)
          for cmd = (format nil "move-window-to-group-set ~A" gs)
          collect (list (tkbd key) cmd))))


(defvar *top-map-qwerty*
  (make-interactive-keymap top-map-qwerty (:exit-on ((tkbd "s-T")))
    ((tkbd "s-h") "prev-in-frame")
    ((tkbd "s-l") "next-in-frame")
    ((tkbd "s-k") "prev-frame-or-window")
    ((tkbd "s-j") "next-frame-or-window")

    ((tkbd "s-H") "move-window left")
    ((tkbd "s-L") "move-window right")
    ((tkbd "s-K") "move-window up")
    ((tkbd "s-J") "move-window down")

    ((tkbd "s-s") "iresize-qwerty")
    ((tkbd "s-g") "imove-qwerty")))


(defvar *top-map-colemak-dh*
  (make-interactive-keymap top-map-colemak-dh (:exit-on ((tkbd "s-T")))
    ((tkbd "s-k") "prev-in-frame")
    ((tkbd "s-i") "next-in-frame")
    ((tkbd "s-e") "prev-frame-or-window")
    ((tkbd "s-n") "next-frame-or-window")

    ((tkbd "s-K") "move-window left")
    ((tkbd "s-I") "move-window right")
    ((tkbd "s-E") "move-window up")
    ((tkbd "s-N") "move-window down")

    ((tkbd "s-s") "iresize-colemak-dh")
    ((tkbd "s-g") "imove-colemak-dh")))


(let ((common-maps (append *common-top-maps* *group-set-maps*)))
  (mapc #'(lambda (k) (define-key *top-map-qwerty* (car k) (cadr k)))
        common-maps)
  (mapc #'(lambda (k) (define-key *top-map-colemak-dh* (car k) (cadr k)))
        common-maps))


(defun iresize-abort-p ()
  (when (and (null (current-window)) (typep (current-group) 'float-group))
    (message "No current window!")
    t))

(define-interactive-keymap iresize-qwerty (:abort-if #'iresize-abort-p)
  ((tkbd "Up")    "resize-any-window up")
  ((tkbd "k")     "resize-any-window up")

  ((tkbd "Down")  "resize-any-window down")
  ((tkbd "j")     "resize-any-window down")

  ((tkbd "Left")  "resize-any-window left")
  ((tkbd "h")     "resize-any-window left")

  ((tkbd "Right") "resize-any-window right")
  ((tkbd "l")     "resize-any-window right"))

(define-interactive-keymap iresize-colemak-dh (:abort-if #'iresize-abort-p)
  ((tkbd "Up")    "resize-any-window up")
  ((tkbd "e")     "resize-any-window up")

  ((tkbd "Down")  "resize-any-window down")
  ((tkbd "n")     "resize-any-window down")

  ((tkbd "Left")  "resize-any-window left")
  ((tkbd "k")     "resize-any-window left")

  ((tkbd "Right") "resize-any-window right")
  ((tkbd "i")     "resize-any-window right"))


(defun imove-abort-p ()
  (when (null (current-window))
    (message "No current window!")
    t))

(define-interactive-keymap imove-qwerty (:abort-if #'imove-abort-p)
  ((tkbd "Up")    "move-any-window up")
  ((tkbd "k")     "move-any-window up")

  ((tkbd "Down")  "move-any-window down")
  ((tkbd "j")     "move-any-window down")

  ((tkbd "Left")  "move-any-window left")
  ((tkbd "h")     "move-any-window left")

  ((tkbd "Right") "move-any-window right")
  ((tkbd "l")     "move-any-window right"))

(define-interactive-keymap imove-colemak-dh (:abort-if #'imove-abort-p)
  ((tkbd "Up")    "move-any-window up")
  ((tkbd "e")     "move-any-window up")

  ((tkbd "Down")  "move-any-window down")
  ((tkbd "n")     "move-any-window down")

  ((tkbd "Left")  "move-any-window left")
  ((tkbd "k")     "move-any-window left")

  ((tkbd "Right") "move-any-window right")
  ((tkbd "i")     "move-any-window right"))

