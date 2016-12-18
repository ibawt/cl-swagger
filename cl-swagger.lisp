;;;; cl-swagger.lisp
;;;;
;;;; Copyright (c) 2016 Ian Quick

(in-package #:cl-swagger)

(defun fetch-json (url)
  (multiple-value-bind (body response-code)
      (drakma:http-request url :want-stream t)
    (setf (flex:flexi-stream-external-format body) :utf-8)
    (ecase response-code
      (200 (cl-json:decode-json body)))))

(defun get-in (items alist)
  (if (endp items) alist
      (get-in (rest items)
              (cdr (assoc (car items) alist)))))

(defparameter *test-doc* (fetch-json "https://api.thetvdb.com/swagger.json"))
(defparameter *test-path* (assoc :/search/series (cdr (assoc :paths *test-doc*))))
(defparameter *test-def* (assoc :*token (cdr (assoc :definitions *test-doc*))))

(defstruct swagger
  version
  info
  host
  base-path
  schemes
  consumes
  produces
  paths
  definitions
  parameters
  responses
  security-definitions
  security
  tags)

(defclass swagger-object ()
  ()
  (:documentation "Base class for swagger objects"))

(defmacro alist-value (item alist)
  `(cdr (assoc ,item ,alist :test #'string=)))

(defun normalize-path-name (name)
  (string-upcase name))

(defun uncamelize (x)
  "returns a new string with uppercase chars converted to -<lower case char>"
  (loop for c across x
        with o = (make-array (length x)
                             :element-type 'character
                             :adjustable t :fill-pointer 0)
        do (if (upper-case-p c)
               (progn
                 (when (and (> (length o) 0)
                            (not (eq #\-
                                    (aref o (1- (length o))))))
                   (vector-push-extend #\- o))
                 (vector-push-extend (char-downcase c) o))
               (vector-push-extend c o))
        finally (return (string-upcase o))))

(defun trim-string (prefix s)
  (subseq s (length prefix)))

(defun gen-parameters (root params)
  (declare (ignore root))
  (mapcar (lambda (x)
            (intern
             (cl-json:simplified-camel-case-to-lisp (trim-string "#/parameters/" (cdr (assoc :$ref x))))))
          params))

(defun gen-url (root path)
  `(format nil "~A://~A/~A/~A"
           ,(or (cdr (assoc :scheme (cdr path)))
                (cadr (assoc :schemes root)))
           *api-host*
           ,(cdr (assoc :base-path root))
           ,(string-downcase (car path))))

(defvar *jwt-token* nil)

(defun resolve-reference (root path)
  ;; TODO: correctly handle the # part
  (get-in (mapcar (lambda (x)
                    (intern (cl-json:camel-case-to-lisp x) "KEYWORD"))
                  (cdr (cl-ppcre:split "/" path)))
          root))

(defun gen-http-headers (root path-method)
  (let ((headers '()))
    ;; TODO: handle different security thingies
    (let* ((sec (caaar (get-in '(:security) path-method)))
           (sec-defs (get-in (list :security-definitions sec) root)))
        (when (string= "header" (get-in '(:in) sec-defs))
          (setf headers `(acons "Authorization" (format nil "Bearer ~A" *jwt-token*) nil)))
    ;; go through each parameter and see if any are headers
    (loop for ((key . val)) in (get-in '(:parameters) path-method)
          do (let ((param (if (string= "$REF" key)
                              (resolve-reference root val)
                              val)))
               (when (string= "header" (get-in '(:in) param))
                 (setf headers  `(acons ,(get-in '(:name) param)
                                        ,(intern (cl-json:simplified-camel-case-to-lisp
                                                  (get-in '(:name) param)))
                                        ,headers))))))
    headers))

(defun gen-http-params (root path)
  (loop for ((key . val)) in (get-in '(:parameters) path)
        with x = nil
        do (let ((param (if (string= "$REF" key)
                            (resolve-reference root val)
                            val)))
             (when (string= "query" (get-in '(:in) param))
               (setf x `(acons ,(get-in '(:name) param)
                               ,(intern (cl-json:simplified-camel-case-to-lisp
                                         (get-in '(:name) param)))
                               ,x))))
        finally (return x)))

(defun gen-http-options (root path)
  `(:parameters ,(gen-http-params root path)
    :additional-headers ,(gen-http-headers root path)
    :want-stream t))

(defun sym-to-integer (x)
  (parse-integer (remove-if (lambda (x)
                              (equal #\: x))
                            (string x))))

(defun make-response (root value)
  (declare (ignore root value))
  :placeholder)

(defun make-struct-name (keyword)
  (subseq (string keyword) 1))

(defun to-symbol (x)
  (intern (uncamelize (string-downcase (string x)))))

(defun strip-star (x)
  (remove-if (lambda (x) (equal #\* x)) x))

(defun gen-slot (root class-name prop)
  (declare (ignore root))
  (let* ((sym-name (to-symbol (strip-star (string (car prop))))))
    `(,sym-name
      :initarg ,(intern (string sym-name) "KEYWORD")
      ,@(list :type (alexandria:switch ((get-in '(:type) (cdr prop)) :test #'equal)
           ("string" 'string)
           ("integer" 'integer)
           ("object" 'swagger-object)))
      ,@(alexandria:if-let ((default (get-in '(:default) (cdr prop))))
         `(:initform ,default))
      :accessor ,(intern (format nil "~A-~A" class-name (strip-star (string (car prop))))))))

(defun gen-definiton (root defn)
  (let* ((class-name (make-struct-name (car defn))))
   `(defclass ,(intern class-name) (swagger-object)
      ,(mapcar (lambda (x)
                 (gen-slot root class-name x)) (get-in '(:properties) (cdr defn))))))

(define-condition unhandled-response-code-error (error)
  ((response-code :initarg :response-code)))

(defmacro sformat (fmt &rest rest)
  `(format nil ,fmt ,@rest))

(defun gen-error (defn)
  `(define-condition ,(intern (sformat "~A-ERROR" (string (car defn))))
       (:description ,(get-in '(:description) (cdr defn)))
     ,(eswitch ((get-in '(:type) (cdr defn)) :test #'equal)
        ("array" '(items :initarg :items))
        ("string" '(what :initarg :what)))))

(defun gen-all-definitions (root)
  (loop for defn in (get-in '(:definitions) root)
        with out = '()
        do (if (equal :+JSON+-ERRORS (car defn))
               (loop for error in (get-in '(:properties) (cdr defn))
                     do (setf out (cons (gen-error error) out)))
               (setf out (cons (gen-definiton root defn) out)))
        finally (return out)))

(defun gen-path-func (root path)
  `(defun ,(intern (normalize-path-name (car path)))
       ,(gen-parameters  root (get-in '(:get :parameters) (cdr path)))
     (with-multiple-value-bind (stream response-code headers)
       (http-request ,(gen-url root path) ,@(gen-http-options root (cdr (assoc :get (cdr path)))))
       (declare (ignore headers))
       (setf (flex:flexi-stream-external-format stream) :utf-8)
       (case response-code
         ,@(loop for (resp . data) in (get-in '(:get :responses) (cdr path))
                 collect (progn
                           `(,(sym-to-integer resp)
                             (make-response root val))))
         otherwise
         (error 'unhandled-response-code-error :text "Invalid response code"
          :response-code response-code)))))
;;; "cl-swagger" goes here. Hacks and glory await!
