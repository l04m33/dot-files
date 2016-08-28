(setq ac-slime-packages '(ac-slime))

(defun ac-slime/init-ac-slime ()
  (use-package ac-slime
    :defer t
    :init
    (progn
      (add-hook 'slime-mode-hook
                #'(lambda ()
                    (set-up-slime-ac t)
                    (auto-complete-mode)))
      (add-hook 'slime-repl-mode-hook
                #'(lambda ()
                    (set-up-slime-ac t)
                    (auto-complete-mode)))
      (eval-after-load "auto-complete"
        '(add-to-list 'ac-modes 'slime-repl-mode)))))
