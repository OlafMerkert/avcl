(in-package :ol-user)

(defmacro! defclass/q (name slots
                      print-form)
  (let ((slot-names (mapcar #'first slots)))
   `(progn
      (defclass ,name ()
        ,(mapcar (lambda (slot)
                   (destructuring-bind (slot-name slot-type &key default) slot
                     (declare (ignorable slot-type))
                     `(,slot-name :initarg ,(keyw slot-name)
                                  :accessor ,slot-name
                                  ,@(if default `(:initform ,default)))))
                 slots))
      (defmethod print-object ((,g!object ,name) ,g!stream)
        (print-unreadable-object (,g!object ,g!stream :type t)
          (with-slots ,slot-names ,g!object
            (declare (ignorable ,@slot-names))
            ,@(mapcar #`(princ ,a1 ,g!stream) print-form)))))))

(defclass/q taetigkeit
    ((titel string)
     (bedarf integer)
     (dozent string)
     (bereich (from "Grundstudium"
                    "Aufbaustudium"
                    "Masterstudium"))
     (termin string)
     (bemerkung string))
  (titel " : " dozent " [" bedarf "]"))

(defclass/q assistent
    ((name string)
     (bedarf integer))
  (name))

(defclass/q wunsch
    ((assistent assistent)
     (taetigkeit taetigkeit)
     (staerke (from 1 2 3 4)))
  ((name assistent) " wuenscht " (titel taetigkeit) " [W" staerke "]"))

(defclass/q zuweisung
    ((assistent assistent)
     (taetigkeit taetigkeit)
     (score integer :default 0)
     (fest boolean))
  ((name assistent) " uebernimmt " (titel taetigkeit) (if fest " [fest]" "")))
