(in-package #:group-set)


(defvar *gset-registry* (make-hash-table))


(defvar *max-gset-number* 0)

(defun new-gset-number ()
  (incf *max-gset-number*))


(defclass group-set ()
  ((number
     :initform 0
     :initarg :number
     :accessor gset-number)
   (name
     :initform nil
     :initarg :name
     :accessor gset-name)
   (current-group
     :initform nil
     :accessor gset-current-group)
   (groups
     :initform nil
     :initarg :groups
     :accessor gset-groups)
   (screen
     :initform nil
     :initarg :screen
     :accessor gset-screen)))


(defun build-groups (screen gset group-specs)
  (let ((gset-name (gset-name gset)))
    (loop for spec in group-specs
          collect (cond
                    ((listp spec)
                     (let ((name (format nil "~A:~A" gset-name (car spec))) (type (cadr spec)))
                       (add-group screen name :background t :type type)))
                    ((stringp spec)
                     (add-group screen (format nil "~A:~A" gset-name spec) :background t))
                    (t (error (format nil "Bad group spec: ~A" spec)))))))


(defun screen-gset-hash (screen)
  (let* ((screen-id (stumpwm::screen-id screen))
         (gset-hash (gethash screen-id *gset-registry*)))
    (unless gset-hash
      (setf gset-hash (make-hash-table :test 'equal))
      (setf (gethash screen-id *gset-registry*) gset-hash))
    gset-hash))


(defun add-group-set (screen name group-specs)
  (let* ((gset-hash (screen-gset-hash screen)))
    (when (gethash name gset-hash)
      (error (format nil "Group set '~A' exists" name)))
    (let* ((gset (make-instance 'group-set
                                :number (new-gset-number)
                                :name name
                                :screen screen))
           (groups (build-groups screen gset group-specs)))
      (unless groups
        (error "Empty group-specs"))
      (setf (gset-groups gset) groups
            (gset-current-group gset) 0
            (gethash name gset-hash) gset)
      gset)))


(defun switch-to-group-set (gset &optional group-nr)
  (if group-nr
    (setf (gset-current-group gset) group-nr)
    (setf group-nr (gset-current-group gset)))
  (let ((cur-group (nth group-nr (gset-groups gset))))
    (stumpwm::switch-to-group cur-group)))


(defun group-gset (group)
  (let* ((screen (group-screen group))
         (gset-hash (screen-gset-hash screen))
         (gset-list (alexandria:hash-table-values gset-hash))
         (res nil))
    (dolist (gset gset-list)
      (when (remove-if-not #'(lambda (g)
                               (eql (group-number g)
                                    (group-number group)))
                           (gset-groups gset))
        (setf res gset)
        (return)))
    res))


(defun current-gset ()
  (group-gset (current-group)))


(defun split-string-by-one-space (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))


(defun split-string-by-space (string)
  (let ((s-list (split-string-by-one-space string)))
    (remove-if #'(lambda (s) (zerop (length s))) s-list)))


(defcommand gset-new (name specs) ((:string "Name of the new group set: ")
                                   (:rest "Group specs: "))
  (let* ((spec-list (split-string-by-space specs))
         (group-specs (loop for i from 0 to (1- (length spec-list)) by 2
                            collect `(,(elt spec-list i)
                                      ,(if (equal (elt spec-list (1+ i)) "float")
                                         'stumpwm.floating-group:float-group
                                         'stumpwm::tile-group)))))
    (add-group-set (current-screen) name group-specs)))


(defcommand gset-select (name &optional group-nr) ((:string "Select group set: ") :string)
  (let* ((gset-hash (screen-gset-hash (current-screen)))
         (gset (gethash name gset-hash)))
    (if gset
      (if group-nr
        (switch-to-group-set gset (parse-integer group-nr))
        (switch-to-group-set gset))
      (message "Group set '~A' not found" name))))
