(defpackage :avcl-ui-entry
  (:use :cl :ol-utils)
  (:export))

(in-package :avcl-ui-entry)

;;; Class save-next-dialog ---------------------------------------------------

(defqclass save-next-dialog (q-widget)
  ((button-box :accessor button-box))
  (:signals save                        ; todo use mklist on these
            close)
  (:slots next-clicked
          save-clicked
          cancel-clicked))

(defgeneric init (qclass))

(defmethod init :after ((save-next-dialog save-next-dialog) &key hide-next)
  (qlayout save-next-dialog v-box
           :stretch
           (setup-buttons save-next-dialog hide-next)))

(defmethod setup-buttons ((save-next-dialog save-next-dialog) hide-next)
  (let ((next       (make-qinstance 'push-button :text "Speichern && Weiter"))
        (save       (make-qinstance 'push-button :text "Speichern"))
        (cancel     (make-qinstance 'push-button :text "Abbrechen"))
        (button-box (make-qinstance 'widget)))
    (qlayout button-box h-box
             :stretch next save cancel)
    (qconnect next clicked
              save-next-dialog next-clicked)
    (qconnect save clicked
              save-next-dialog save-clicked)
    (qconnect cancel clicked
              save-next-dialog cancel-clicked)
    (when hide-next
      (q hide next))
    (setf (button-box save-next-dialog)
          button-box)))


(defmethod next-clicked ((save-next-dialog save-next-dialog))
  (emit save-next-dialog save))

(defmethod save-clicked ((save-next-dialog save-next-dialog))
  (emit save-next-dialog save)
  (emit save-next-dialog close))

(defmethod cancel-clicked ((save-next-dialog save-next-dialog))
  (emit save-next-dialog close))

;;; Class entry-form dialog --------------------------------------------------

(defqclass entry-form-dialog (save-next-dialog)
  ()
  (:signals (new-data ; todo should be given an alist or similar
             )))

(defmethod init :after ((entry-form-dialog entry-form-dialog) &key title))

;;; TODO approach the form generation maybe a little differently???
