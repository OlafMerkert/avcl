(defpackage :avcl-views
  (:use :cl :ol-utils :qt-utils :qt)
  (:export))

(in-package :avcl-views)

(defqclass custom-tree-model (q-abstract-item-model)
  ((header :accessor header
           :initarg  :header
           :initform nil)
   (tree :accessor tree
         :initarg  :tree
         :initform nil))
  (:override row-count
             column-count
             parent
             index
             data
             flags
             header-data))

(defmethod create-index ((self custom-tree-model) row column &optional pointer)
  (q create-index self row column pointer))

(defmethod create-invalid-index (self)
  (make-qinstance 'model-index))

#|(defmethod create-invalid-index ((self custom-tree-model))
  (create-invalid-index t))|#

(defmethod initialize-instance :after ((custom-tree-model custom-tree-model) &key)
  (qt:new custom-tree-model))

(defmethod row-count ((self custom-tree-model) &optional parent)
  (if (q is-valid parent)
      (let ((node (q internal-pointer parent)))
        (if node 1 0))
      (if (tree self) 1 0)))

(defmethod column-count ((self custom-tree-model) &optional parent)
  1)

(defmethod parent ((self custom-tree-model) child))

(defmethod index ((self custom-tree-model) row column &optional parent)
  (if (q is-valid parent)
      (let ((node (q internal-pointer parent)))
        (create-index self row column )
        )))

(defmethod data ((self custom-tree-model) index &optional role)
  (when (and (or (null role) (eql role 0))    ; DisplayRole
           (q is-valid index))
    
      ))

(defmethod flags ((self custom-tree-model) index)
  (if (q is-valid index)
      (+ 1                              ; Selectable
         32)                            ; Enabled
      0))

(defmethod header-data ((self custom-tree-model) section orientation &optional role)
  (when (and (or (null role) (eql role 0)) ; DisplayRole
             (eql orientation 1))
    (nth section (header self))))
