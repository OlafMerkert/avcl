(defpackage :avcl-views
  (:use :cl :ol-utils :qt-utils :qt)
  (:export :define-tree-model
           :clear :fetch
           :custom-tree-model
           :custom-tree-view
           :show
           :empty))

(in-package :avcl-views)

(defun empty (x)
  (declare (ignore x))
  "")

;;; the model part
(defqclass custom-tree-model (q-standard-item-model)
  ((root-object :initarg :root :initform nil
                :accessor root-object)))

(defmethod initialize-instance :after ((custom-tree-model custom-tree-model) &key)
  (qt:new custom-tree-model))

(defmethod clear ((custom-tree-model custom-tree-model))
  (q clear custom-tree-model))

(defmethod fetch ((custom-tree-model custom-tree-model) &key root)
  (declare (ignore root))
  (clear custom-tree-model))

(defmacro! define-tree-model (name header &rest levels)
  "TODO"
  (multiple-value-bind (descendors accessors) (splitn levels 2)
    (let ((gdescendors (list->gensyms descendors))
          (gaccessors (mapcar #'list->gensyms accessors)))
      `(let (,@(mapcar #'list gdescendors descendors)
             ,@(mapcar #'list (flatten1 gaccessors) (flatten1 accessors)))
         (defqclass ,name (custom-tree-model)
           ())

         (defmethod fetch ((model ,name) &key (root nil root-supplied-p))
           (clear model)
           (when root-supplied-p
             (setf (root-object model)
                   root))
           ;; create the header
           ,@(mapcar (lambda (i h)
                       `(q set-horizontal-header-item model ,i
                           (make-qinstance 'standard-item ,h)))
                     (lrange header) header)
           ;; put the standard-item models in there
           ,(labels
             ((add-items (qitem item descendors accessors)
                         (when descendors
                           (with-gensyms! 
                             `(dolist (,g!r (funcall ,(first descendors) ,item))
                                (let (,g!qr)
                                  ,@(create-row qitem g!qr g!r (first accessors))
                                  ,(add-items g!qr g!r (rest descendors) (rest accessors)))))))
              (create-row (qroot qitem item accessors)
                          (with-gensyms!
                            `((setf ,qitem (make-qinstance 'standard-item
                                                           (format nil "~A"
                                                                   (funcall ,(first accessors) ,item))))
                              (q append-row ,qroot
                                 (list ,qitem
                                       ,@(mapcar #`(make-qinstance 'standard-item
                                                                   (format nil "~A" (funcall ,a1 ,item)))
                                                 (rest accessors))))))))
             (add-items `(q invisible-root-item model)
                        `(root-object model)
                        gdescendors
                        gaccessors)))))))
;;; the view part
(defqclass custom-tree-view (q-tree-view)
  ((expand-p :accessor expand-p
             :initarg  :expand
             :initform t)))

(defmethod initialize-instance :after ((custom-tree-view custom-tree-view) &key model)
  (qt:new custom-tree-view)
  (setf (q model custom-tree-view) model)
  (fetch model)
  (expand custom-tree-view))

(defmethod show ((custom-tree-view custom-tree-view))
  (q show custom-tree-view))


(defgeneric expand (view &key expand))

(defmethod expand ((custom-tree-view custom-tree-view) &key (expand t supplied-p))
  (if (if supplied-p expand (expand-p custom-tree-view))
      (q expand-all custom-tree-view)
      (q collapse-all custom-tree-view)))

