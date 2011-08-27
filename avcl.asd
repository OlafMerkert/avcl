(defsystem avcl
  :serial t
  :depends-on ("ol-utils" "qt-utils" "qt" "cl-prevalence")
  :components ((:module "ui"
                        :serial t
                        :components ((:file "forms")))
               (:file "models")))
