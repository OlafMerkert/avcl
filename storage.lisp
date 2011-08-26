(defpackage :avcl-storage
  (:use :cl :ol-utils :cl-prevalence)
  (:export :*default-store*
           :save-data))

(use-package :cl-prevalence)

;;; Benutze cl-prevalence, um daten zu speichern
(cl-prevalence:find-all-objects *default-store* 'assistent)

(defparameter *storage-dir* #P"~/.avcl/data/")

(defparameter *default-store*
  (make-prevalence-system *storage-dir*))

(defun save-data ()
  (snapshot *default-store*))

(setf (get-root-object *default-store* :assistenten)
      (list (make-instance 'assistent :name "Olaf Merkert")))

(setf (get-root-object *default-store* :taetigkeiten)
      (list (make-instance 'taetigkeit
                           :titel "Algebra"
                           :dozent "David Masser"
                           :bedarf 2
                           :bereich "Aufbaustudium")))

