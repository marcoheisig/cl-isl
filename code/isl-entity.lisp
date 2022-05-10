(in-package :cl-isl)

(defgeneric isl-entity-plist (isl-entity))

(defgeneric copy (isl-entity))

(defstruct (isl-entity (:copier nil))
  (handle (alexandria:required-argument :handle)
   :type cffi:foreign-pointer))

(defmethod print-object ((isl-entity isl-entity) stream)
  (print-unreadable-object (isl-entity stream :type t)
    (format stream "~@<~{~S ~S~^ ~_~}~:>"
            (isl-entity-plist isl-entity))))

(defmethod isl-entity-plist ((isl-entity isl-entity))
  (declare (ignore isl-entity))
  '())

(defmacro isl-entity-%copy (entity-name)
  "Returns the name of the function for creating a fresh copy of the
handle of the ISL entity denoted by ENTITY-NAME."
  `(getf (symbol-plist ,entity-name) '%copy))

(defmacro isl-entity-%make (entity-name)
  "Returns the name of the function for turning a suitable handle into an
ISL entity of type ENTITY-NAME."
  `(getf (symbol-plist ,entity-name) '%make))

(defmacro isl-entity-%free (entity-name)
  "Returns the name of the function for freeing a handle of an ISL entity
of type ENTITY-NAME."
  `(getf (symbol-plist ,entity-name) '%free))

(defun isl-entity-name-p (x)
  (and (symbolp x)
       (not (null (isl-entity-%make x)))))

;;; This hash table is used in each ISL entity constructor to ensure that
;;; each handle has exactly one corresponding wrapper object.
(defvar *isl-entity-table* (trivial-garbage:make-weak-hash-table :weakness :value))

(defmacro define-isl-entity
    (name &key (abstract nil)
            (superclass 'isl-entity)
            ((:free %free) (isl-entity-%free superclass))
            ((:copy %copy) (isl-entity-%copy superclass)))
  (let ((predicate (make-isl-sym name (if (find #\- (string name)) "-P" "P")))
        (%make (make-isl-sym "%MAKE-" name))
        (%%make (make-isl-sym "%%MAKE-" name)))
    (setf (isl-entity-%copy name) %copy)
    (setf (isl-entity-%make name) %make)
    (setf (isl-entity-%free name) %free)
    `(progn
       (defstruct (,name (:include ,superclass)
                         (:predicate ,predicate)
                         (:copier nil)
                         (:constructor ,%%make (handle))))
       (declaim (ftype (function (cffi:foreign-pointer) (values ,name &optional)) ,%make))
       ,@(unless abstract
           `((defun ,%make (handle)
               (values
              ;  (alexandria:ensure-gethash
              ;   (cffi:pointer-address handle)
              ;   *isl-entity-table*
                 (trivial-garbage:finalize (,%%make handle)
                                           (lambda ()
                                           ;  (setf (gethash
                                           ;         (cffi:pointer-address handle)
                                           ;         *isl-entity-table*)
                                           ;        nil)
                                           ;  (,%free handle)
                                             ))))))
       ,@(when %copy
           `((defmethod copy ((,name ,name))
               (,%make (,%copy (isl-entity-handle ,name)))))))))
