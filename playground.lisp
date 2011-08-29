(defpackage :avcl-playground
  (:use :cl :ol-utils :qt-utils :avcl-models :avcl-storage :avcl-views)
  (:export :put-data))

(in-package :avcl-playground)

;;; put some initial test data in the collection
(defun put-data ()
  (labels ((put (key inst)
             (multiple-value-bind (col new) (collection key)
               (when new
                 (add inst col)))))
    (let ((olaf (make-instance 'assistent :name "Olaf Merkert"))
          (algebra (make-instance 'taetigkeit
                          :titel "Algebra"
                          :dozent "David Masser"
                          :bedarf 2
                          :bereich "Aufbaustudium")))
      (put :assistenten olaf)
      (put :taetigkeiten algebra)
      (put :wuensche (make-instance 'wunsch :assistent olaf
                                    :taetigkeit algebra
                                    :staerke 1))
      (put :zuweisungen (make-instance 'zuweisung :assistent olaf
                                       :taetigkeit algebra
                                       :score 39129)))))
