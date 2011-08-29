(defpackage :avcl-views
  (:use :cl :ol-utils :qt-utils :qt)
  (:export :define-tree-model
           :clear :fetch))

(in-package :avcl-views)

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
