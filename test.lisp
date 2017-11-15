(defpackage :test
  (:use :cl :cl-swagger))

(in-package :test)

(defparameter *petstore-url* "http://localhost/v2/swagger.json")
(defvar *petstore-json* (let* ((cl-json:*json-identifier-name-to-lisp* #'identity))
                         (fetch-json *petstore-url*)))
(time (generate-client :test *k8s-json*))

(defvar *k8s-json* (fetch-json "https://raw.githubusercontent.com/kubernetes/kubernetes/master/api/openapi-spec/swagger.json"))

(defvar *foo*)

(setf *foo* (generate-client :test *petstore-json*))

(defmacro foobar ()
  (let ((x (generate-client :test *petstore-json*)))
    x))

(defmacro foobar-2 ()
  (let ((x (generate-client :test *k8s-json*)))
    x))

(foobar)

(foobar-2)

(setf swagger:*api-host* "localhost")
