(defpackage #:custom-utils
  (:nicknames #:cutil)
  (:use #:cl
        #:stumpwm)

  (:export #:build-resource-dir))


(in-package #:custom-utils)


(defun build-resource-dir (&rest components)
  (let* ((rel-modules-dir
           (make-pathname
             :directory (append '(:relative) components))))
    (merge-pathnames rel-modules-dir *data-dir*)))

