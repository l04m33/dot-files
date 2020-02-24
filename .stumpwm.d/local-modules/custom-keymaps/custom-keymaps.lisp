(defpackage #:custom-keymaps
  (:nicknames #:ckmap)
  (:use #:cl
        #:stumpwm)
  (:import-from #:stumpwm
                #:tile-group
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

    (,(kbd "s-,") "hsplit-and-focus")
    (,(kbd "s-.") "vsplit-and-focus")
    (,(kbd "s-=") "balance-frames")

    (,(kbd "s-f") "fullscreen")

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

    ((kbd "s-s") "iresize")))


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

    ((kbd "s-s") "iresize-colemak-dh")))


(let ((common-maps (append *common-top-maps* *group-set-maps*)))
  (mapc #'(lambda (k) (define-key *top-map-qwerty* (car k) (cadr k)))
        common-maps)
  (mapc #'(lambda (k) (define-key *top-map-colemak-dh* (car k) (cadr k)))
        common-maps))


(define-interactive-keymap (iresize-colemak-dh tile-group) (:on-enter #'setup-iresize
                                                            :on-exit #'resize-unhide
                                                            :abort-if #'abort-resize-p)
  ((kbd "Up")    "resize-direction up")
  ((kbd "e")     "resize-direction up")

  ((kbd "Down")  "resize-direction down")
  ((kbd "n")     "resize-direction down")

  ((kbd "Left")  "resize-direction left")
  ((kbd "k")     "resize-direction left")

  ((kbd "Right") "resize-direction right")
  ((kbd "i")     "resize-direction right"))

