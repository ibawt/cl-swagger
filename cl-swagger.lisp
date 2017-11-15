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

(defvar *target-package* nil
  "package to intern things into")

(defclass swagger-object ()
  ()
  (:documentation "Base class for swagger objects"))
(export 'swagger-object)

(defun decode-json (x)
  "decodes json without transforming keys"
  (let ((cl-json:*json-identifier-name-to-lisp* #'identity))
    (cl-json:decode-json x)))

(defmacro simplified-camel-case-to-lisp (x)
  "see the matching cl-json method"
  `(cl-json:simplified-camel-case-to-lisp ,x))

(defun json->lisp (x &key (keyword))
  "json key to a more lispy name"
  (let ((name (simplified-camel-case-to-lisp (string x))))
    (if keyword
        (intern name :keyword)
        (intern name *target-package*))))

(defun fetch-json (url)
  "grabs json from the http url"
  (multiple-value-bind (body response-code)
      (drakma:http-request url :want-stream t)
    (setf (flex:flexi-stream-external-format body) :utf-8)
    (ecase response-code
      (200 (decode-json body)))))
(export 'fetch-json)

(defun get-in (items alist)
  "Calls assoc on alist for each thing in items. Returns the cdr of that"
  (loop for x in items
        with l = alist
        do (setf l (cdr (assoc x l)))
        finally (return l)))

(defmacro get-in2 (items alist)
  (if (listp items)
   (loop for x in items
         with o = alist
         do (setf o `(cdr (assoc ,x ,o)))
         finally (return o))
   `(cdr (assoc ,items ,alist))))

(defun gen-parameters (root params)
  (declare (ignore root))
  (mapcar (lambda (x)
            (json->lisp (get-in2 :|name| x)))
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
  (loop :for e in (split-path path)
        :if (paramp e) collect e into params
          :else collect e into path-elems
        finally (return (values path-elems params))))

(defun split-path (p)
  "splits the path by / and removes empty entries"
  (split-sequence:split-sequence #\/ p :remove-empty-subseqs t))

(defun gen-url (root path verb)
  "generates the url for the path and verb"
  (let ((param-elements (nth-value 1 (path-parameters  (string path))))
        (scheme (or (cadr (assoc :|schemes| (cdr verb)))
                    (cadr (assoc :|schemes| root))))
        (base-path (or (get-in2 :|basePath| root)
                       "")))
    `(format nil
             ,(format nil "~~A://~~A~A/~{~A~^/~}" base-path
                      (mapcar
                       (lambda (x) (if (paramp x) "~A" x))
                       (split-path (string path))))
             (or *api-scheme* ,scheme)
             ,(if-let ((host (get-in2 :|host| root)))
                `(or *api-host* ,host)
                '*api-host*)
             ,@(mapcar (lambda (x)
                         (json->lisp (strip-braces x)))
                       param-elements))))

(defun resolve-reference (root path)
  ;; TODO: correctly handle the # part
  (get-in (mapcar (lambda (x)
                    (intern x :keyword))
                  (cdr (cl-ppcre:split "/" path)))
          root))

(defun gen-http-headers (root path-method)
  "returns headers for path and method"
  (let (headers)
    ;; TODO: handle different security thingies
    (let* ((sec (caaar (get-in2 :|security| path-method)))
           (sec-defs (get-in (list :|security-definitions| sec) root)))
        (when (string= "header" (get-in '(:|in|) sec-defs))
          (setf headers `(acons "Authorization" (format nil "Bearer ~A" *jwt-token*) nil))))
    ;; go through each parameter and see if any are headers
    (dolist (p (get-in2 :|parameters| path-method))
      (let ((param (if-let (resolved (resolve-reference root (get-in '(:|$ref|) p)))
                     resolved
                     p)))
        (if (string= "header" (get-in2 :|in| param))
            (setf headers `(acons ,(get-in2 :|name| param)
                                  ,(json->lisp (get-in2 :|name| param))
                                  ,headers)))))
    headers))

(defun gen-http-params (root path)
  "returns query params for path"
  (declare (ignore root))
  (reduce (lambda (acc p)
            (if (string= "query" (get-in2 :|in| p))
                `(acons ,(get-in2 :|name| p)
                        ,(json->lisp (get-in2 :|name| p))
                        ,acc)
                acc))
          (get-in2 :|parameters| path)
          :initial-value nil))

(defun gen-http-body (root path)
  "returns a list of :content and a body for http bodies"
  (declare (ignore root))
  (loop for p in (get-in2 :|parameters| path) do
    (if (string= "body" (get-in2 :|in| p))
        (return `(:content (cl-json:encode-json-to-string (to-json ,(json->lisp (get-in2 :|name| p)))))))))

(defun gen-http-options (root path)
  "for inclusion in the final http request"
  `(:parameters ,(gen-http-params root path)
    ,@(gen-http-body root path)
    :additional-headers ,(gen-http-headers root path)
    :want-stream t))

(defun sym-to-integer (x)
  "returns the integer from the symbol, ie: :|200| -> 200"
  (parse-integer (string x)))

(defun model-json-name (model-name)
  (json->lisp (format nil "~A-FROM-JSON" model-name)))

(defun schema->model (value)
  (car (last (cl-ppcre:split "/" (or (get-in2 (:|schema| :|items| :|$ref|) value)
                                     (get-in2 (:|schema| :|$ref|) value))))))

(defun make-response (root value)
  "how to generate the response from path"
  (declare (ignore root))
  (let* ((type (get-in2 (:|schema| :|type|) value))
         (model-name (schema->model value))
         (from-json (model-json-name model-name)))
    (cond
      ((string= "array" type)
       `(mapcar (lambda (item) (,from-json item)) json))
      ((string= "object" type) 'json)
      ((get-in2 (:|schema| :|$ref|) value) `(,from-json json))
      (t (get-in2 :|description| value)))))

(defun prop->slot (x)
  "property to slot name"
  (json->lisp (string (car x))))

(defun prop->accessor (prop class-name)
  "property to slot accessor"
  (json->lisp (format nil "~A-~A" class-name (car prop))))

(defun gen-slot (root class-name prop)
  "generates a slot definition from the property"
  (declare (ignore root))
  (let ((sym-name (prop->slot prop)))
    `(,sym-name
      :initarg ,(json->lisp (string sym-name) :keyword t)
      ,@(list :type (switch ((get-in2 :|type| (cdr prop)) :test #'equal)
                      ("string" 'string)
                      ("array" 'list)
                      ("integer" 'integer)
                      ("object" 'swagger-object)))
      ,@(if-let ((default (get-in2 :default (cdr prop))))
          `(:initform ,default))
      :accessor ,(prop->accessor prop class-name))))

(defgeneric properties (object)
  (:documentation "returns the swagger properties"))

(defun gen-definiton (root defn)
  "generates a class  and methods for the swagger class definition"
  (let ((class-name (json->lisp (car defn)))
        (properties (get-in2 :|properties| (cdr defn))))
    `((defclass ,class-name (swagger:swagger-object)
        ,(mapcar (lambda (x) (gen-slot root class-name x)) properties))
        ,@(if-let ((doc (get-in2 :|description| (cdr defn))))
            `(:documentation ,doc))
      (defmethod xml-name ((o ,class-name))
        ,(get-in2 (:|xml| :|name|) (cdr defn)))
      (defmethod properties ((o ,class-name))
        ',properties)
      (defun ,(json->lisp (format nil "~A-FROM-JSON" class-name)) (alist)
        (make-instance ',class-name
                       ,@(mapcan (lambda (p)
                                   `(,(intern (string (prop->slot p)) :keyword) (get-in '(,(car p)) alist)))
                                 properties)))
      (defmethod to-json ((k ,class-name))
        ,(switch ((get-in2 :|type| (cdr defn)) :test #'string=)
           ("object" (reduce (lambda (acc p)
                               `(acons ,(string (car p))
                                       (,(prop->accessor p class-name) k)
                                       ,acc))
                             properties
                             :initial-value nil)))))))

(defun gen-error (defn)
  "generates a condition for the swagger error"
  `(define-condition ,(json->lisp (format nil "~A-ERROR" (string (car defn)))) (error)
     ,(eswitch ((get-in2 :type (cdr defn)) :test #'equal)
        ("array" '((items :initarg :items)))
        ("string" '((what :initarg :what))))))

(defun gen-all-definitions (root)
  "loops over all definitions"
  (loop for defn in (get-in2 :|definitions| root)
        append (gen-definiton root defn)))

(defun gen-all-paths (root)
  (loop for path in (get-in2 :|paths| root)
        append (mapcar
                (lambda (verb)
                  (gen-path-func root (car path) verb))
                (remove nil (mapcar (lambda (x)
                                      (assoc x (cdr path)))
                                    '(:|get| :|put| :|post| :|delete|
                                      :|options| :|head| :|patch|))))))

(defun gen-path-func (root path verb)
  `(defun ,(if-let ( (x (json->lisp (get-in2 :|operationId| (cdr verb)))) )
             x
             (break))
       ,(gen-parameters root (get-in2 :|parameters| (cdr verb)))
     ,(get-in2 :|description| (cdr verb))
     (multiple-value-bind (body-stream response-code headers)
         (http-request ,(gen-url root path verb) ,@(gen-http-options root (cdr verb))
                       :method ,(json->lisp (car verb) :keyword t))
       (setf (flex:flexi-stream-external-format body-stream) :utf-8)
       (unless (string= (get-in2 :content-type headers)
                        "application/json")
         (error 'swagger:unknown-response-content-type-error
                :content-type (cdr (assoc "Content-Type" headers))))
       (let ((json (decode-json body-stream)))
         (pprint json)
         (case response-code
           ,@(loop for (resp . data) in (get-in2 :|responses| (cdr verb))
                   collect (list (if (equal :|default| resp)
                                     'otherwise
                                     (sym-to-integer resp))
                                 (make-response root data))))))))

(define-condition package-not-found-error (error)
  ((package :initarg :package)))

(defun generate-client (package json)
  (let* ((old-package (package-name *package*))
         (new-pkg (find-package package))
         (*target-package* new-pkg)
         out)
  (unless new-pkg
    (error 'package-not-found-error :package package))
    (setf *package* new-pkg)
    (unwind-protect
         (setf out `(progn
                      (defvar *api-host*)
                      ,@(gen-all-definitions json)
                      ,@(gen-all-paths json))))
    (setf *package* (find-package old-package))
    out))
(export 'generate-client)
