;;;; cl-swagger.asd
;;;;
;;;; Copyright (c) 2016 Ian Quick

(asdf:defsystem #:cl-swagger
  :description "Describe cl-swagger here"
  :author "Ian Quick <ian.quick@gmail.com>"
  :license "AGPL"
  :serial t
  :depends-on (:alexandria
               :cl-json
               :drakma)
  :components ((:file "package")
               (:file "cl-swagger")))
