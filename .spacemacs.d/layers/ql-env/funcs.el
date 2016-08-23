(defun spacemacs//ql-env-find-env-dirs (envs-path)
  (if (file-directory-p envs-path)
    (cl-loop for f in (directory-files-and-attributes
                       (expand-file-name envs-path) nil "^[^.]")
             for setup-file = (and (cadr f)
                                   (locate-file
                                    "setup"
                                    (list (concat envs-path (car f) "/"))
                                    '(".lisp")))
             when setup-file collect (cons (car f) setup-file))
    nil))

(defun spacemacs/ql-env-build-impl-list (cmd-args envs-path)
  (cl-loop for (env-name . setup-file) in (spacemacs//ql-env-find-env-dirs
                                           envs-path)
           collect `(,(intern (concat (car cmd-args) "-" env-name))
                     ,(append (copy-list cmd-args) (list setup-file))
                     :coding-system utf-8-unix)))
