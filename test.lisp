(defpackage :test
  (:use :cl :cl-swagger))

(in-package :test)

(defparameter *petstore-url* "http://localhost/v2/swagger.json")
(defvar *petstore-json* (let* ((cl-json:*json-identifier-name-to-lisp* #'identity))
                         (fetch-json *petstore-url*)))
;; (generate-client :test "https://raw.githubusercontent.com/kubernetes/kubernetes/master/api/openapi-spec/swagger.json")

(generate-client :test "http://localhost:80/v2/swagger.json")

(setf swagger:*api-host* "localhost")
