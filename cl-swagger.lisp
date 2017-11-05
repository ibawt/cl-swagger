;;;; cl-swagger.lisp
;;;;
;;;; Copyright (c) 2016 Ian Quick

(in-package #:cl-swagger)

(defvar *api-host* nil)
(export '*api-host*)

;;;-----------------------------------
;;; Debug
(defun get-slots (object)
  "gets slot names"
  ;; thanks to cl-prevalence
  #+openmcl
  (mapcar #'ccl:slot-definition-name
          (#-openmcl-native-threads ccl:class-instance-slots
           #+openmcl-native-threads ccl:class-slots
           (class-of object)))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object)))
  #+lispworks
  (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+clisp
  (mapcar #'clos:slot-definition-name (clos:class-slots (class-of object)))
  #-(or openmcl cmu lispworks allegro sbcl clisp)
  (error "not yet implemented"))

(set-macro-character
 #\{
 #'(lambda (str char)
     (declare (ignore char))
     (let ((list (read-delimited-list #\} str t)))
       (let ((type (first list))
             (list (second list)))
         (let ((class (allocate-instance (find-class type))))
           (loop for i in list do
             (setf (slot-value class (car i)) (cdr i)))
           class)))))

(defmethod print-object ((object standard-object) stream)
  (print-unreadable-object (object stream :type t)
      (format stream "~a"
           (loop for i in (get-slots object)
                 collect (cons i (slot-value object i))))))

;;-----------------------------------------------------------

(define-condition unhandled-response-code-error (error)
  ((response-code :initarg :response-code))
  (:documentation "A condition that is raised when the response code doesn't match any in the path definitons"))
(export 'unhandled-response-code-error)

(define-condition unknown-response-content-type-error (error)
  ((content-type :initarg :content-type))
  (:documentation "A condition that is raised when the content type doesn't match application/json"))
(export 'unknown-response-content-type-error)

(defclass swagger-object ()
  ()
  (:documentation "Base class for swagger objects"))
(export 'swagger-object)

(defun decode-json (x)
  "decodes json without transforming keys"
  (let ((cl-json:*json-identifier-name-to-lisp* #'identity))
    (cl-json:decode-json x)))

(defun simplified-camel-case-to-lisp (x)
  "see the matching cl-json method"
  (cl-json:simplified-camel-case-to-lisp x))

(defun json->lisp (x &key (keyword))
  "json key to a more lispy name"
  (let ((name (simplified-camel-case-to-lisp (string x))))
    (if keyword
        (intern name :keyword)
        (intern name))))

(defun fetch-json (url)
  "grabs json from the http url"
  (multiple-value-bind (body response-code)
      (drakma:http-request url :want-stream t)
    (setf (flex:flexi-stream-external-format body) :utf-8)
    (ecase response-code
      (200 (decode-json body)))))
(export 'fetch-json)

(defmethod get-in (items alist)
  "Calls assoc on alist for each thing in items. Returns the cdr of that"
  (reduce (lambda (acc x)
            (cdr (assoc x acc)))
          items :initial-value alist))

(defun gen-parameters (root params)
  (declare (ignore root))
  (mapcar (lambda (x)
            (json->lisp
             (if-let (name (get-in '(:|name|) x))
                name
                (car (last (cl-ppcre:split "/" (if-let ((ref (get-in '(:$ref) x)))
                                                 ref
                                                 (get-in '(:schema :$ref) x))))))))
          params))

(defun paramp (x)
  "is this string a swagger path parameter?"
  (equal #\{ (elt x 0)))

(defun strip-braces (x)
  "removes open and closed braces from the sequence"
  (remove-if (lambda (e)
               (or (equal #\{ e) (equal #\} e)))
             x))

(defun path-parameters (path)
  "splits the / delimited path into params and constants,
   returns multiple values (constant path elements, parameter path elements)"
  (let (path-elems params)
    (loop :for e in (split-path path)
          :do  (cond
                 ((paramp e) (push (strip-braces e) params))
                 (t (push e path-elems))))
    (values (nreverse path-elems) (nreverse params))))

(defun split-path (p)
  "splits the path by / and removes empty entries"
  (remove-if #'emptyp (cl-ppcre:split "\\/" p)))

(defun gen-url (root path verb)
  "generates the url for the path and verb"
  (let ((param-elements
          (nth-value 1 (path-parameters  (string path)))))
    (let* ((scheme (or (cdr (assoc :|scheme| (cdr verb)))
                       (cadr (assoc :|schemes| root))))
           (base-path (get-in '(:|basePath|) root)))
      `(format nil
               ,(format nil "~A://~~A~A/~{~A~^/~}" scheme base-path
                        (mapcar
                         (lambda (x) (if (paramp x) "~A" x))
                         (split-path (string path))))
               (or *api-host* ,(get-in '(:|host|) root))
               ,@(mapcar #'json->lisp param-elements)))))

(defun resolve-reference (root path)
  ;; TODO: correctly handle the # part
  (get-in (mapcar (lambda (x)
                    (intern (cl-json:camel-case-to-lisp x) "KEYWORD"))
                  (cdr (cl-ppcre:split "/" path)))
          root))

(defun gen-http-headers (root path-method)
  (let (headers)
    ;; TODO: handle different security thingies
    (let* ((sec (caaar (get-in '(:|security|) path-method)))
           (sec-defs (get-in (list :|security-definitions| sec) root)))
        (when (string= "header" (get-in '(:|in|) sec-defs))
          (setf headers `(acons "Authorization" (format nil "Bearer ~A" *jwt-token*) nil))))
    ;; go through each parameter and see if any are headers
    (dolist (p (get-in '(:|parameters|) path-method))
      (let ((param (if-let (resolved (resolve-reference root (get-in '(:$ref) p)))
                     resolved
                     p)))
        (if (string= "header" (get-in '(:|in|) param))
            (setf headers `(acons ,(get-in '(:|name|) param)
                                  ,(json->lisp (get-in '(:|name|) param))
                                  ,headers)))))
    headers))

(defun gen-http-params (root path)
  (declare (ignore root))
  (let (result)
    (dolist (p (get-in '(:|parameters|) path))
        (if (string= "query" (get-in '(:|in|) p))
            (setf result `(acons ,(get-in '(:|name|) p)
                                 ,(->param (get-in '(:|name|) p))
                                 ,result))))
    result))

(defun gen-http-body (root path)
  (declare (ignore root))
  (loop for p in (get-in '(:|parameters|) path)
        do (if (string= "body" (get-in '(:|in|) p))
               (return `(:content (cl-json:encode-json-to-string (to-json ,(json->lisp (get-in '(:|name|) p)))))))))

(defun gen-http-options (root path)
  `(:parameters ,(gen-http-params root path)
    ,@(gen-http-body root path)
    :additional-headers ,(gen-http-headers root path)
    :want-stream t))

(defun sym-to-integer (x)
  (parse-integer (string x)))

(defun make-response (root value)
  (declare (ignore root))
  (let ((type (get-in '(:|schema| :|type|) value)))
    (cond
      ((string= "array" type) (let ((model-name (car (last (cl-ppcre:split "/" (get-in '(:|schema| :|items| :|$ref|) value))))))
                `(loop for item in json
                       collect (,(json->lisp (format nil "~A-FROM-JSON" model-name)) item))))
      ((string= "object" type)
       'json)
      ((get-in '(:|schema| :|$ref|) value)
       (let ((model-name (car (last (cl-ppcre:split "/" (get-in '(:|schema| :|$ref|) value))))))
         `(,(json->lisp (format nil "~A-FROM-JSON" model-name)) json)))
      (t (get-in '(:|description|) value)))))

(defun prop->slot (x)
  (json->lisp (string (car x))))

(defun prop->accessor (prop class-name)
  (json->lisp (format nil "~A-~A" class-name (car prop))))

(defun gen-slot (root class-name prop)
  (declare (ignore root))
  (let ((sym-name (prop->slot prop)))
    `(,sym-name
      :initarg ,(json->lisp (string sym-name) :keyword t)
      ,@(list :type (switch ((get-in '(:|type|) (cdr prop)) :test #'equal)
                      ("string" 'string)
                      ("array" 'list)
                      ("integer" 'integer)
                      ("object" 'swagger-object)))
      ,@(if-let ((default (get-in '(:default) (cdr prop))))
          `(:initform ,default))
      :accessor ,(prop->accessor prop class-name))))

(defgeneric properties (object)
  (:documentation "returns the swagger properties"))

(defun gen-definiton (root defn)
  (let ((class-name (json->lisp (car defn))))
    `((defclass ,class-name (swagger:swagger-object)
        ,(mapcar (lambda (x)
                   (gen-slot root class-name x))
          (get-in '(:|properties|) (cdr defn)))
        ,@(if-let ((doc (get-in '(:|description|) (cdr defn))))
            `(:documentation ,doc)))
      (defmethod xml-name ((o ,class-name))
        ,(get-in '(:|xml| :|name|) (cdr defn)))
      (defmethod properties ((o ,class-name))
        ',(get-in '(:|properties|) (cdr defn)))
      (defun ,(json->lisp (format nil "~A-FROM-JSON" class-name)) (alist)
        (make-instance ',class-name
                       ,@(loop for p in (get-in '(:|properties|) (cdr defn))
                               append `(,(intern (string (prop->slot p)) "KEYWORD") (get-in '(,(car p)) alist)))))
      (defmethod to-json ((k ,class-name))
        ,(if (string= "object" (get-in '(:|type|) (cdr defn)))
             (let (result)
               (loop for p in (get-in '(:|properties|) (cdr defn))
                     do (setf  result `(acons ,(string (car p))
                                              (,(prop->accessor p class-name) k)
                                              ,result)))
               result))))))

(defun gen-error (defn)
  `(define-condition ,(json->lisp (format nil "~A-ERROR" (string (car defn)))) (error)
     ,(eswitch ((get-in '(:type) (cdr defn)) :test #'equal)
        ("array" '((items :initarg :items)))
        ("string" '((what :initarg :what))))))

(defun gen-all-definitions (root)
  (let (out)
    (loop for defn in (get-in '(:|definitions|) root)
          do (let ((d
                     (if (equal :+JSON+-ERRORS (car defn))
                         (loop for error in (get-in '(:|properties|) (cdr defn))
                               do (gen-error error))
                         (gen-definiton root defn))))
               (setf out (append out d))))
    out))

(defun gen-all-paths (root)
  (mapcan (lambda (path)
            (mapcar (lambda (verb)
                      (gen-path-func root (car path) verb))
                    (cdr path)))
          (get-in '(:|paths|) root)))

(defun gen-path-func (root path verb)
  `(defun ,(json->lisp (get-in '(:|operationId|) (cdr verb)))
       ,(gen-parameters  root (get-in '(:|parameters|) (cdr verb)))
     ,(get-in '(:|description|) (cdr verb))
     (multiple-value-bind (body-stream response-code headers)
         (http-request ,(gen-url root path verb) ,@(gen-http-options root (cdr verb))
                       :method ,(json->lisp (car verb) :keyword t))
       (setf (flex:flexi-stream-external-format body-stream) :utf-8)
       ;; (format t "response_code: ~A" response-code)
       ;; (pprint headers)
       (unless (string= (get-in '(:content-type) headers)
                        "application/json")
         (error 'swagger:unknown-response-content-type-error
                :content-type (get-in '("Content-Type") headers)))
       (let ((json (decode-json body-stream)))
         (pprint json)
         (case response-code
           ,@(loop for (resp . data) in (get-in '(:|responses|) (cdr verb))
                   collect (list (if (equal :|default| resp)
                                     'otherwise
                                     (sym-to-integer resp))
                                 (make-response root data))))))))

(define-condition package-not-found-error (error)
  ((package :initarg :package)))

(defmacro generate-client (package url)
  (let* ((json (fetch-json url))
         (old-package (package-name *package*))
         (new-pkg (find-package package))
         out)
  (unless new-pkg
    (error 'package-not-found-error :package package))
    (setf *package* new-pkg)
    (unwind-protect
         (setf out `(progn
                      ,@(gen-all-definitions json)
                      ,@(gen-all-paths json))))
    (setf *package* (find-package old-package))
    out))
(export 'generate-client)
