;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Ian Quick

(defpackage #:cl-swagger
  (:use #:cl
        #:alexandria
        #:drakma)
  (:nicknames #:swagger)
  (:export
   #:generate-client
   #:swagger-object
   #:unknown-response-content-type-error
   #:unhandled-response-code-error))
