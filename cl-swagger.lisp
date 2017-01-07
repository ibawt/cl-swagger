;;;; cl-swagger.lisp
;;;;
;;;; Copyright (c) 2016 Ian Quick

(in-package #:cl-swagger)

(define-condition unhandled-response-code-error (error)
  ((response-code :initarg :response-code)))
(export 'unhandled-response-code-error)

(define-condition unknown-response-content-type-error (error)
  ((content-type :initarg :content-type)))
(export 'unknown-response-content-type-error)

(defclass swagger-object ()
  ()
  (:documentation "Base class for swagger objects"))
(export 'swagger-object)

(defun fetch-json (url)
  (multiple-value-bind (body response-code)
      (drakma:http-request url :want-stream t)
    (setf (flex:flexi-stream-external-format body) :utf-8)
    (ecase response-code
      (200 (cl-json:decode-json body)))))

(defun get-in (items alist)
  "Calls assoc on alist for each thing in items. Returns the cdr of that"
  (if (endp items) alist
      (get-in (rest items)
              (cdr (assoc (car items) alist)))))

(defparameter *test-doc* (fetch-json "https://api.thetvdb.com/swagger.json"))
(defparameter *test-path* (assoc :/search/series (cdr (assoc :paths *test-doc*))))
(defparameter *test-def* (assoc :*token (cdr (assoc :definitions *test-doc*))))


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

(defparameter *test-path-parameter* "/foo/{item}/{bar}")

(defun parse-path-parameters (path)
  "returns two values, first is non param path element, second are the params"
  (values-list (mapcar #'nreverse
                       (reduce
                        (lambda (acc v)
                          (if (string= "" v)
                              acc
                              (let ((param (cl-ppcre:register-groups-bind (param)
                                               (*parameter-pattern* v) param)))
                                (if param
                                    (list (first acc) (push param (second acc)))
                                    (list (push v (first acc)) (second acc))))))
                        (cl-ppcre:split "/" (string path))
                        :initial-value (list nil nil)))))

(defun normalize-path-name (name)
  (string-upcase (format nil "~{~A~^-~}" (parse-path-parameters name))))

(defun trim-string (prefix s)
  (subseq s (length prefix)))

(defun gen-parameters (root params)
  (declare (ignore root))
  (mapcar (lambda (x)
            (intern
             (simplified-camel-case-to-lisp
              (car (last (cl-ppcre:split "/" (if-let ((ref (get-in '(:$ref) x)))
                                               ref
                                               (get-in '(:schema :$ref) x))))))))
          params))

(defparameter *parameter-pattern* "{([A-Z\-]+)}")

(defun path-parameters (path)
  (let (result)
    (cl-ppcre:do-register-groups (param) (*parameter-pattern* path)
      (push param result))
    (nreverse result)))

(defun gen-url (root path verb)
  (multiple-value-bind (path-elements param-elements)
      (parse-path-parameters  path)
    (let* ((scheme (or (cdr (assoc :scheme (cdr verb)))
                       (cadr (assoc :schemes root))))
           (base-path (get-in '(:base-path) root))
           (fmt-params (nconc path-elements (mapcar (lambda (x)
                                                      (declare (ignore x))
                                                      "~A")
                                                    param-elements))))
      `(format nil
               ,(if (string= "/" base-path)
                    (format nil "~A://~~A/~{~A/~}" scheme fmt-params)
                    (format nil "~A://~~A/~A/~{~A/~}" scheme
                            base-path fmt-params))
               *api-host*
               ,@(mapcar #'intern param-elements)))))

(defvar *jwt-token* nil)

(defun resolve-reference (root path)
  ;; TODO: correctly handle the # part
  (get-in (mapcar (lambda (x)
                    (intern (cl-json:camel-case-to-lisp x) "KEYWORD"))
                  (cdr (cl-ppcre:split "/" path)))
          root))

(defun gen-http-headers (root path-method)
  (let (headers)
    ;; TODO: handle different security thingies
    (let* ((sec (caaar (get-in '(:security) path-method)))
           (sec-defs (get-in (list :security-definitions sec) root)))
        (when (string= "header" (get-in '(:in) sec-defs))
          (setf headers `(acons "Authorization" (format nil "Bearer ~A" *jwt-token*) nil))))
    ;; go through each parameter and see if any are headers
    (dolist (p (get-in '(:parameters) path-method))
      (let ((param (if-let (resolved (resolve-reference root (get-in '(:$ref) p)))
                     resolved
                     p)))
        (if (string= "header" (get-in '(:in) param))
            (setf headers `(acons ,(get-in '(:name) param)
                                  ,(intern (simplified-camel-case-to-lisp
                                            (get-in '(:name) param)))
                                  ,headers)))))
    headers))

(defun gen-http-params (root path)
  (let (result)
    (dolist (p (get-in '(:parameters) path))
      (let ((param (if-let (resolved (resolve-reference root (get-in '(:$ref) p)))
                     resolved
                     p)))
        (if (string= "query" (get-in '(:in) param))
            (setf result `(acons ,(get-in '(:name) param)
                                        ,(intern (simplified-camel-case-to-lisp
                                                  (get-in '(:name) param)))
                                        ,result)))))
    result))

(defun gen-http-options (root path)
  `(:parameters ,(gen-http-params root path)
    :additional-headers ,(gen-http-headers root path)
    :want-stream t))

(defun sym-to-integer (x)
  (parse-integer (remove-if (lambda (x)
                              (equal #\: x))
                            (string x))))

(defun make-response (root value)
  (declare (ignore root))
  (let ((model-name (car (last (cl-ppcre:split "/" (get-in '(:schema :$ref) value))))) )
    (if model-name
        `(make-instance ',(intern (simplified-camel-case-to-lisp model-name)))
        t)))

(defun make-struct-name (keyword)
  (subseq (string keyword) 1))

(defun to-symbol (x)
  (intern (uncamelize (string-downcase (string x)))))

(defun strip-star (x)
  (remove-if (curry #'equal #\*) x))

(defun gen-slot (root class-name prop)
  (declare (ignore root))
  (let* ((sym-name (to-symbol (strip-star (string (car prop))))))
    `(,sym-name
      :initarg ,(intern (string sym-name) "KEYWORD")
      ,@(list :type (switch ((get-in '(:type) (cdr prop)) :test #'equal)
           ("string" 'string)
           ("integer" 'integer)
           ("object" 'swagger-object)))
      ,@(if-let ((default (get-in '(:default) (cdr prop))))
         `(:initform ,default))
      :accessor ,(intern (format nil "~A-~A" class-name (strip-star (string (car prop))))))))

(defun gen-definiton (root defn)
  (let* ((class-name (make-struct-name (car defn))))
   `(defclass ,(intern class-name) (swagger:swagger-object)
      ,(mapcar (lambda (x)
                 (gen-slot root class-name x)) (get-in '(:properties) (cdr defn)))
      ,@(if-let ((doc (get-in '(:description) (cdr defn))))
        `(:documentation ,doc)))))

(defmacro sformat (fmt &rest rest)
  `(format nil ,fmt ,@rest))

(defun gen-error (defn)
  `(define-condition ,(intern (sformat "~A-ERROR" (string (car defn)))) (error)
     ,(eswitch ((get-in '(:type) (cdr defn)) :test #'equal)
        ("array" '((items :initarg :items)))
        ("string" '((what :initarg :what))))))

(defun gen-all-definitions (root)
  (let (out)
    (loop for defn in (get-in '(:definitions) root)
          do (if (equal :+JSON+-ERRORS (car defn))
                 (loop for error in (get-in '(:properties) (cdr defn))
                       do (push (gen-error error) out))
                 (push (gen-definiton root defn) out)))
    out))

(defun gen-all-paths (root)
  (mapcan (lambda (path)
            (mapcar (lambda (verb)
                      (gen-path-func root (car path) verb))
                    (cdr path)))
          (cdr (assoc :paths root))))

(defun gen-path-func (root path verb)
  `(defun ,(intern (format nil "~A-~A" (normalize-path-name path) (car verb)))
       ,(gen-parameters  root (get-in '(:parameters) (cdr verb)))
     ,(get-in '(:description) (cdr verb))
     (multiple-value-bind (body-stream response-code headers)
         (http-request ,(gen-url root path verb) ,@(gen-http-options root (cdr verb))
                       :method ,(car verb))
       (setf (flex:flexi-stream-external-format body-stream) :utf-8)
       (unless (string= (get-in '("Content-Type") headers)
                        "application/json")
         (error 'swagger:unknown-response-content-type-error
                :content-type (get-in '("Content-Type") headers)))
       (let ((json (cl-json:decode-json body-stream)))
         (case response-code
           ,@(loop for (resp . data) in (get-in '(:responses) (cdr verb))
                   collect (list (sym-to-integer resp) (make-response root data)))
           (otherwise
            (error 'swagger:unhandled-response-code-error :text "Invalid response code"
                                                          :response-code response-code)))))))

(define-condition package-not-found-error (error)
  ((package :initarg :package)))

(defmacro generate-client (package url)
  (let ((json (fetch-json url))
        (old-package (package-name *package*))
        out)
    (unless old-package
      (error 'package-not-found-error :package package))
    (setf *package* (find-package package))
    (unwind-protect
         (setf out `(progn
                      (defvar *jwt-token* nil)
                      (defvar *api-host* nil)
                      ,@(gen-all-definitions json)
                      ,@(gen-all-paths json))))
    (setf *package* (find-package old-package))
    out))
(export 'generate-client)
