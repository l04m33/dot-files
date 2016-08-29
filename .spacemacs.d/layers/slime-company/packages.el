(setq slime-company-packages '(slime-company))

(defun slime-company/init-slime-company ()
  (use-package slime-company
    :defer t
    :init
    (progn
      (add-hook 'slime-mode-hook #'(lambda () (company-mode)))
      (add-hook 'slime-repl-mode-hook #'(lambda () (company-mode)))
      (eval-after-load "slime"
        '(add-to-list 'slime-contribs 'slime-company)))))
