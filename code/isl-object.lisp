(in-package :cl-isl)

(defgeneric copy (isl-object))

(defstruct (isl-object (:copier nil))
  (handle (alexandria:required-argument :handle)
   :type cffi:foreign-pointer))

(defmethod print-object ((isl-object isl-object) stream)
  (print-unreadable-object (isl-object stream :type t)
    (format stream "{~X}" (cffi:pointer-address (isl-object-handle isl-object)))))

(defmacro isl-object-%copy (object-name)
  "Returns the name of the function for creating a fresh copy of the
handle of the ISL object denoted by OBJECT-NAME."
  `(getf (symbol-plist ,object-name) '%copy))

(defmacro isl-object-%make (object-name)
  "Returns the name of the function for turning a suitable handle into an
ISL object of type OBJECT-NAME."
  `(getf (symbol-plist ,object-name) '%make))

(defmacro isl-object-%free (object-name)
  "Returns the name of the function for freeing a handle of an ISL object
of type OBJECT-NAME."
  `(getf (symbol-plist ,object-name) '%free))

(defun isl-object-name-p (x)
  (and (symbolp x)
       (not (null (isl-object-%make x)))))

;;; This hash table is used in each ISL object constructor to ensure that
;;; each handle has exactly one corresponding wrapper object.
(defvar *isl-object-table* (trivial-garbage:make-weak-hash-table :weakness :value))

(defmacro define-isl-object
    (name &key (abstract nil)
            (superclass 'isl-object)
            ((:free %free) (isl-object-%free superclass))
            ((:copy %copy) (isl-object-%copy superclass)))
  (let ((predicate (make-isl-sym name (if (find #\- (string name)) "-P" "P")))
        (%make (make-isl-sym "%MAKE-" name))
        (%%make (make-isl-sym "%%MAKE-" name)))
    (setf (isl-object-%copy name) %copy)
    (setf (isl-object-%make name) %make)
    (setf (isl-object-%free name) %free)
    `(progn
       (defstruct (,name (:include ,superclass)
                         (:predicate ,predicate)
                         (:copier nil)
                         (:constructor ,%%make (handle))))
       (declaim (ftype (function (cffi:foreign-pointer) (values ,name &optional)) ,%make))
       ,@(unless abstract
           `((defun ,%make (handle)
               (values
                (alexandria:ensure-gethash
                 (cffi:pointer-address handle)
                 *isl-object-table*
                 (trivial-garbage:finalize (,%%make handle)
                                           (lambda ()
                                             (remhash (cffi:pointer-address handle) *isl-object-table*)
                                             (,%free handle))))))))
       ,@(when %copy
           `((defmethod copy ((,name ,name))
               (,%make (,%copy (isl-object-handle ,name)))))))))
