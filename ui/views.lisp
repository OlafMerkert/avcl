(defpackage :avcl-views
  (:use :cl :ol-utils :qt-utils :qt)
  (:export :define-tree-model
           :clear :fetch
           :create-item :item->object
           :custom-tree-model
           :custom-tree-view
           :show
           :empty
           :model-action-view))

(in-package :avcl-views)

(defun empty (x)
  (declare (ignore x))
  "")

;;; the model part
(defqclass custom-tree-model (q-standard-item-model)
  ((root-object :initarg :root :initform nil
                :accessor root-object)
   ;; this hash table is used for mapping items to objects
   (item-table :initform (make-hash-table)
               :reader table)))

(defmethod initialize-instance :after ((custom-tree-model custom-tree-model) &key)
  (qt:new custom-tree-model))

(defmethod clear ((custom-tree-model custom-tree-model))
  (q clear custom-tree-model)
  (clrhash (table custom-tree-model)))

(defmethod fetch ((custom-tree-model custom-tree-model) &key root)
  (declare (ignore root))
  (clear custom-tree-model))

(defmethod create-item ((custom-tree-model custom-tree-model) content &optional (object nil supplied-p))
  (let ((item (make-qinstance 'standard-item
                              (format nil "~A" content))))
    (when supplied-p
      (setf (gethash item (table custom-tree-model))
            object))
    item))

(defmethod item->object ((custom-tree-model custom-tree-model) item)
  (gethash item (table custom-tree-model)))

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
                            `((setf ,qitem (create-item model
                                                        (funcall ,(first accessors) ,item)
                                                        ,item))
                              (q append-row ,qroot
                                 (list ,qitem
                                       ,@(mapcar #`(create-item model 
                                                                (funcall ,a1 ,item))
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

(defmethod get-selected ((custom-tree-view custom-tree-view))
  (let* ((model (q model custom-tree-view))
         (index (q current-index (q selection-model custom-tree-view)))
         (item  (q item-from-index model index)))
    (item->object model item)))

;;; Define a view with actions -- that is, a number of buttons at the top

(defqclass slotter (q-object)
  ((slot-fun :initarg :fun))
  (:slots (call (lambda (this) (funcall (slot-value this 'slot-fun))))))

(defmethod initialize-instance :after ((slotter slotter) &key)
  (qt:new slotter))

(defmacro! model-action-view ((var model) &rest buttons)
  (let ((button-syms (list->gensyms :button buttons))
        (slot-syms   (list->gensyms :slot   buttons)))
    `(let* ((,var ,model) ; model
            (,g!view (make-instance 'custom-tree-view :model ,var))
            (,g!widget (make-qinstance 'widget))
            (,g!button-box (make-qinstance 'widget)) ; container for buttons
            ,@(mapcar #2`(,a1 (make-qinstance 'push-button ,(first a2)))
                      button-syms buttons)
            ,@(mapcar #2`(,a1 (make-instance 'slotter
                                             :fun (lambda ()
                                                    (funcall ,(second a2)
                                                             (get-selected ,g!view)))))
                      slot-syms buttons))
       ;; lay it out
       (qlayout ,g!button-box h-box ,@button-syms :stretch)
       (qlayout ,g!widget v-box ,g!button-box ,g!view)
       ;; connect the signals
       ,@(mapcar #2`(qconnect ,a1 clicked ,a2 call) button-syms slot-syms)
       (values ,g!widget ,g!view ,var))))
