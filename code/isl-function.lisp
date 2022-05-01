(in-package #:cl-isl)

(defgeneric isl-arg-name (isl-arg))

(defgeneric isl-arg-type (isl-arg))

(defclass isl-arg ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :reader isl-arg-name)
   (%type
    :initarg :type
    :initform (alexandria:required-argument :type)
    :reader isl-arg-type)))

(defclass isl-entity-arg (isl-arg)
  ())

(defclass isl-keep (isl-entity-arg)
  ())

(defclass isl-take (isl-entity-arg)
  ())

(defmethod print-object ((isl-arg isl-arg) stream)
  (print-unreadable-object (isl-arg stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (isl-arg-name isl-arg)
            :type (isl-arg-type isl-arg))))

(defvar *generate-arg-name*)

(defun generate-arg-name (type)
  (funcall *generate-arg-name* type))

(defun make-arg-name-generator ()
  (let ((table (make-hash-table :test #'equal)))
    (lambda (type)
      (let* ((prefix
               (etypecase type
                 ;; Symbols
                 (symbol (symbol-name type))
                 ;; Types like (unsigned-byte 32).
                 ((cons symbol (cons integer null))
                  (format nil "~A-~D"
                          (symbol-name (first type))
                          (second type)))
                 (t "ARG")))
             (count (incf (gethash prefix table -1)))
             (suffix (if (zerop count) "" (format nil "~D" count))))
        (make-isl-sym prefix suffix)))))

(defun parse-isl-args (arg-specs)
  (let ((*generate-arg-name* (make-arg-name-generator)))
    (mapcar #'parse-isl-arg arg-specs)))

(defun parse-isl-arg (spec)
  (assert (consp spec))
  (let ((type (car spec)))
    (if (isl-entity-name-p type)
        (destructuring-bind (kind &optional (name (generate-arg-name type)))
            (rest spec)
          (ecase kind
            (:take (make-instance 'isl-take :type type :name name))
            (:keep (make-instance 'isl-keep :type type :name name))))
        (destructuring-bind (&optional (name (generate-arg-name type)))
            (rest spec)
          (make-instance 'isl-arg :type type :name name)))))

(defclass isl-fn ()
  (;; The name of this function.
   (%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type symbol
    :reader isl-fn-name)
   ;; The name of the CFFI function that underlies this function.
   (%primitive
    :initarg :primitive
    :initform (alexandria:required-argument :primitive)
    :type symbol
    :reader isl-fn-primitive)
   ;; The type of the sole value returned by that function.
   (%result-type
    :initarg :result-type
    :initform (alexandria:required-argument :result-type)
    :reader isl-fn-result-type)
   ;; The function that turns the primitive's result into a lisp object.
   (%result-wrapper
    :initarg :result-wrapper
    :initform (alexandria:required-argument :result-wrapper)
    :type symbol
    :reader isl-fn-result-wrapper)
   ;; Whether the primitive function expects the current ISL context as a
   ;; first argument.
   (%ctx
    :initarg :ctx
    :initform nil
    :type boolean
    :reader isl-fn-ctx)
   ;; The parsed arguments.
   (%args
    :initarg :args
    :type list
    :reader isl-fn-args)))

(defmethod print-object ((isl-fn isl-fn) stream)
  (print-unreadable-object (isl-fn stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (isl-fn-name isl-fn)
            :primitive (isl-fn-primitive isl-fn)
            :result-type (isl-fn-result-type isl-fn)
            :result-wrapper (isl-fn-result-wrapper isl-fn)
            :ctx (isl-fn-ctx isl-fn)
            :args (isl-fn-args isl-fn))))

(defvar *isl-fns* (make-hash-table :test #'eq))

(defmacro isl-fn (name)
  `(values (gethash ,name *isl-fns*)))

(defmethod initialize-instance :after ((isl-fn isl-fn) &key &allow-other-keys)
  (setf (isl-fn (isl-fn-name isl-fn))
        isl-fn))

(defun infer-result-wrapper (result-type)
  (cond ((isl-entity-name-p result-type)
         (isl-entity-%make result-type))
        ((eql result-type 'boolean)
         'lispify-isl-bool)
        (t 'identity)))

(defmacro define-isl-function
    (name primitive (&key (result-type 'null) (result-wrapper (infer-result-wrapper result-type)) (ctx nil))
     &rest args)
  (check-type name symbol)
  (check-type primitive symbol)
  (with-accessors ((name isl-fn-name)
                   (primitive isl-fn-primitive)
                   (result-type isl-fn-result-type)
                   (result-wrapper isl-fn-result-wrapper)
                   (ctx isl-fn-ctx)
                   (args isl-fn-args))
      (make-instance 'isl-fn
        :name name
        :primitive primitive
        :result-type result-type
        :result-wrapper result-wrapper
        :ctx ctx
        :args (parse-isl-args args))
    `(progn
       (declaim (ftype (function ,(mapcar #'isl-arg-type args) (values ,result-type &optional)) ,name))
       (defun ,name ,(mapcar #'isl-arg-name args)
         (declare
          ,@(loop for arg in args collect `(type ,(isl-arg-type arg) ,(isl-arg-name arg))))
         (,result-wrapper
          (,primitive
           ,@(when ctx '((isl-entity-handle *context*)))
           ,@(loop for arg in args
                   for name = (isl-arg-name arg)
                   for type = (isl-arg-type arg)
                   collect
                   (etypecase arg
                     (isl-keep `(isl-entity-handle ,name))
                     (isl-take `(,(isl-entity-%copy type) (isl-entity-handle ,name)))
                     (isl-arg name))))))
       (define-compiler-macro ,name (&whole whole ,@(mapcar #'isl-arg-name args))
         (declare (ignore ,@(mapcar #'isl-arg-name args)))
         (optimize-isl-function-call whole)))))

(defun constructor-form-p (form isl-entity-name)
  (and (= 2 (length form))
       (eql (first form)
            (isl-entity-%make isl-entity-name))))

(defun creation-form-p (form isl-entity-name)
  (and (consp form)
       (let ((fn (isl-fn (first form))))
         (and fn
              (= (length (rest form))
                 (length (isl-fn-args fn)))
              (eq (isl-fn-result-type fn) isl-entity-name)))))

(defun optimize-isl-function-call (whole &key (recursive nil))
  (destructuring-bind (name &rest forms) whole
    (with-accessors ((name isl-fn-name)
                     (primitive isl-fn-primitive)
                     (result-type isl-fn-result-type)
                     (result-wrapper isl-fn-result-wrapper)
                     (ctx isl-fn-ctx)
                     (args isl-fn-args))
        (isl-fn name)
      (let* ((worth-expanding recursive)
             (bindings '())
             (cleanup '())
             (expanded-arguments
               (loop for arg in args
                     for type = (isl-arg-type arg)
                     for form in forms
                     collect
                     (etypecase arg
                       (isl-take
                        (cond
                          ((constructor-form-p form type)
                           (setf worth-expanding t)
                           (second form))
                          ((creation-form-p form type)
                           (setf worth-expanding t)
                           (optimize-isl-function-call form :recursive t))
                          (t
                           `(,(isl-entity-%copy type)
                             (isl-entity-handle (the ,type ,form))))))
                       (isl-keep
                        (cond
                          ((constructor-form-p form type)
                           (setf worth-expanding t)
                           (let ((handle (gensym)))
                             (push `(,handle ,(second form)) bindings)
                             (push `(,(isl-entity-%free type) ,handle) cleanup)
                             handle))
                          ((creation-form-p form type)
                           (setf worth-expanding t)
                           (let ((handle (gensym)))
                             (push `(,handle ,(optimize-isl-function-call form :recursive t)) bindings)
                             (push `(,(isl-entity-%free type) ,handle) cleanup)
                             handle))
                          (t
                           `(isl-entity-handle (the ,type ,form)))))
                       (isl-arg `(the ,type ,form)))))
             (expansion
               `(,primitive
                 ,@(when ctx '((isl-entity-handle *context*)))
                 ,@expanded-arguments)))
        (when cleanup
          (setf expansion `(unwind-protect ,expansion ,@cleanup)))
        (when bindings
          (setf expansion `(let ,bindings ,expansion)))
        (unless recursive
          (unless (eql result-wrapper 'identity)
            (setf expansion `(,result-wrapper ,expansion))))
        (if (not worth-expanding) whole expansion)))))
