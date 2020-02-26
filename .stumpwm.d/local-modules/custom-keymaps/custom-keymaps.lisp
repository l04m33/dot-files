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
  (:export #:make-interactive-keymap))


(in-package #:custom-keymaps)


(defmacro make-interactive-keymap
    (name (&key on-enter on-exit abort-if (exit-on '((kbd "RET")
                                                     (kbd "ESC")
                                                     (kbd "C-g"))))
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
  `((,(kbd "s-t") ,*root-map*)

    (,(kbd "s-RET") "exec xterm")

    (,(kbd "s-C") "delete-maybe-remove")
    (,(kbd "s-/") "windowlist")
    (,(kbd "s-?") "frame-windowlist")

    (,(kbd "s-,") "hsplit-and-focus")
    (,(kbd "s-.") "vsplit-and-focus")
    (,(kbd "s-=") "balance-frames")

    (,(kbd "s-f") "fullscreen")
    (,(kbd "s-F") "float-this-maybe-remove")

    (,(kbd "s-r") "exec")
    (,(kbd "s-:") "eval")
    (,(kbd "s-;") "colon")

    (,(kbd "s-SPC")   "switch-group-in-group-set")
    (,(kbd "S-s-SPC") "move-all-windows-to-other-group")

    (,(kbd "s-p") "show-group-overview")
    (,(kbd "s-P") "vgroups")
    
    (,(kbd "XF86AudioLowerVolume") "amixer-Master-1-")
    (,(kbd "XF86AudioRaiseVolume") "amixer-Master-1+")
    (,(kbd "XF86AudioMute")        "amixer-Master-toggle")))


(defvar *group-set-maps*
  (append
    (loop for gs from cglobal:*first-group* to cglobal:*last-group*
          for key = (format nil "s-~A" gs)
          for cmd = (format nil "gset-select ~A" gs)
          collect (list (kbd key) cmd))
    (loop for gs from cglobal:*first-group* to cglobal:*last-group*
          for char in '(#\) #\! #\@ #\# #\$ #\% #\^ #\& #\* #\()
          for key = (format nil "s-~A" char)
          for cmd = (format nil "move-window-to-group-set ~A" gs)
          collect (list (kbd key) cmd))))


(defvar *top-map-qwerty*
  (make-interactive-keymap top-map-qwerty (:exit-on ((kbd "C-s-t")))
    ((kbd "s-h") "prev-in-frame")
    ((kbd "s-l") "next-in-frame")
    ((kbd "s-k") "prev-frame-or-window")
    ((kbd "s-j") "next-frame-or-window")

    ((kbd "s-H") "move-window left")
    ((kbd "s-L") "move-window right")
    ((kbd "s-K") "move-window up")
    ((kbd "s-J") "move-window down")

    ((kbd "s-s") "iresize-qwerty")
    ((kbd "s-g") "imove-qwerty")))


(defvar *top-map-colemak-dh*
  (make-interactive-keymap top-map-colemak-dh (:exit-on ((kbd "C-s-t")))
    ((kbd "s-k") "prev-in-frame")
    ((kbd "s-i") "next-in-frame")
    ((kbd "s-e") "prev-frame-or-window")
    ((kbd "s-n") "next-frame-or-window")

    ((kbd "s-K") "move-window left")
    ((kbd "s-I") "move-window right")
    ((kbd "s-E") "move-window up")
    ((kbd "s-N") "move-window down")

    ((kbd "s-s") "iresize-colemak-dh")
    ((kbd "s-g") "imove-colemak-dh")))


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
  ((kbd "Up")    "resize-any-window up")
  ((kbd "k")     "resize-any-window up")

  ((kbd "Down")  "resize-any-window down")
  ((kbd "j")     "resize-any-window down")

  ((kbd "Left")  "resize-any-window left")
  ((kbd "h")     "resize-any-window left")

  ((kbd "Right") "resize-any-window right")
  ((kbd "l")     "resize-any-window right"))

(define-interactive-keymap iresize-colemak-dh (:abort-if #'iresize-abort-p)
  ((kbd "Up")    "resize-any-window up")
  ((kbd "e")     "resize-any-window up")

  ((kbd "Down")  "resize-any-window down")
  ((kbd "n")     "resize-any-window down")

  ((kbd "Left")  "resize-any-window left")
  ((kbd "k")     "resize-any-window left")

  ((kbd "Right") "resize-any-window right")
  ((kbd "i")     "resize-any-window right"))


(defun imove-abort-p ()
  (when (null (current-window))
    (message "No current window!")
    t))

(define-interactive-keymap imove-qwerty (:abort-if #'imove-abort-p)
  ((kbd "Up")    "move-any-window up")
  ((kbd "k")     "move-any-window up")

  ((kbd "Down")  "move-any-window down")
  ((kbd "j")     "move-any-window down")

  ((kbd "Left")  "move-any-window left")
  ((kbd "h")     "move-any-window left")

  ((kbd "Right") "move-any-window right")
  ((kbd "l")     "move-any-window right"))

(define-interactive-keymap imove-colemak-dh (:abort-if #'imove-abort-p)
  ((kbd "Up")    "move-any-window up")
  ((kbd "e")     "move-any-window up")

  ((kbd "Down")  "move-any-window down")
  ((kbd "n")     "move-any-window down")

  ((kbd "Left")  "move-any-window left")
  ((kbd "k")     "move-any-window left")

  ((kbd "Right") "move-any-window right")
  ((kbd "i")     "move-any-window right"))

