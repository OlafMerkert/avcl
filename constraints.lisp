(defpackage :avcl-constraints
  (:use :cl :ol-utils :avcl-models :avcl-storage)
  (:export))

(in-package :avcl-constraints)

(defmacro defzuwmethod (name args &body body)
  `(defmethod ,name (,@args &optional (zuweisungen (->list (collection :zuweisungen))))
     ,@body))


(bind-multi ((item   assistent    taetigkeit)
             (collid :assistenten :taetigkeiten))
  (defmethod unassigned ((who (eql 'item))))
    
  (defzuwmethod all-assigned-p ((who (eql 'item)))
    (every (lambda (a) (assigned-p a zuweisungen)) (->list (collection collid))))

  (defzuwmethod assigned-p ((item item))
    (= (bedarf item)
       (loop for z in zuweisungen
          when (eql item (item z))
          sum 1))))
