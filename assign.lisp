(defpackage :avcl-assign
  (:use :cl :ol-utils :qt-utils
        :avcl-storage :avcl-models :avcl-views)
  (:export))

(in-package :avcl-assign)

;;; Versuche ausgehend von Assistenten zuzuweisen
(define-tree-model assign-by-assistent-model
    ("Name" "Wunsch" "Bedarf")
  (ilambda (x)
    (->list (collection :assistenten)))
  (#'name #'empty #'bedarf)
  #'wuensche
  ((compose #'titel #'taetigkeit) (lambda (x) (format nil "W~A" (staerke x))) (compose #'bedarf #'taetigkeit)))

(defun assign-by-assistent-view ()
  (let* ((m (make-instance 'assign-by-assistent-model))
         (v (make-instance 'custom-tree-view :model m)))
    (show v)))

;;; ausgehend von Vorlesungen
(define-tree-model assign-by-taetigkeit-model
    ("Titel" "Wunsch" "Bedarf")
  (ilambda (x)
    (->list (collection :taetigkeiten)))
  (#'titel #'empty #'bedarf)
  #'wuensche
  ((compose #'name #'assistent) (lambda (x) (format nil "W~A" (staerke x))) (compose #'bedarf #'assistent)))

(defun assign-by-taetigkeit-view ()
  (let* ((m (make-instance 'assign-by-taetigkeit-model))
         (v (make-instance 'custom-tree-view :model m)))
    (show v)))

(defun assign-by-taetigkeit ()
  (multiple-value-bind (widget view model)
      (model-action-view
       (model (make-instance 'assign-by-taetigkeit-model))
       ("Zuweisen" (lambda (item)
                     (format t "~A~%" item)
                                        ; todo
                     ))
       ("Zuruecknehmen" (lambda (item)
                          (format t "~A~%" item)
                                        ; todo
                          )))
    (q show widget)))
