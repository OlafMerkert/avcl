(defpackage :avcl-assign
  (:use :cl :ol-utils :qt-utils
        :avcl-storage :avcl-models :avcl-views)
  (:export))

(in-package :avcl-assign)

(defun staerke->score (x)
  x)

(defmethod zuweisen ((wunsch wunsch))
  (add (make-instance 'zuweisung :wunsch wunsch
                      :score (staerke->score (staerke wunsch)))
       (collection :zuweisungen)))

(defmethod zuruecknehmen ((wunsch wunsch))
  (let ((zuw (zuweisungen wunsch)))
    (del (first zuw) (collection :zuweisungen))))

;;; Versuche ausgehend von Assistenten zuzuweisen
(define-tree-model assign-by-assistent-model
    ("Name" "Wunsch" "Zugewiesen" "Bedarf")
  (ilambda (x)
    (->list (collection :assistenten)))
  (#'name #'empty (lambda (x)
                    (loop for z in (->list (collection :zuweisungen))
                         if (eql (assistent (wunsch z)) x)
                         sum 1))
          #'bedarf)
  #'wuensche
  ((compose #'titel #'taetigkeit) (lambda (x) (format nil "W~A" (staerke x)))
   (lambda (x)
     (length (zuweisungen x)))
   (compose #'bedarf #'taetigkeit)))

(defun assign-by-assistent ()
  (multiple-value-bind (widget view model)
      (model-action-view
       (model (make-instance 'assign-by-assistent-model))
       ("Zuweisen" #'zuweisen)
       ("Zuruecknehmen" #'zuruecknehmen))
    (q show widget)))

;;; ausgehend von Vorlesungen
(define-tree-model assign-by-taetigkeit-model
    ("Titel" "Wunsch" "Bedarf")
  (ilambda (x)
    (->list (collection :taetigkeiten)))
  (#'titel #'empty #'bedarf)
  #'wuensche
  ((compose #'name #'assistent) (lambda (x) (format nil "W~A" (staerke x))) (compose #'bedarf #'assistent)))


(defun assign-by-taetigkeit ()
  (multiple-value-bind (widget view model)
      (model-action-view
       (model (make-instance 'assign-by-taetigkeit-model))
       ("Zuweisen" #'zuweisen)
       ("Zuruecknehmen" #'zuruecknehmen))
    (q show widget)))
