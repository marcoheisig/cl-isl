(in-package :cl-isl)

(defgeneric handle-of (isl-entity))

(defgeneric write-isl-entity (isl-entity stream))

(defgeneric copy-isl-entity (isl-entity))

(defclass isl-entity ()
  ((%handle
    :initarg :handle
    :initform (alexandria:required-argument :handle)
    :type cffi:foreign-pointer
    :reader handle-of)))

(defmethod print-object ((isl-entity isl-entity) stream)
  (print-unreadable-object (isl-entity stream :type t)
    (write-isl-entity isl-entity stream)))

(defmethod write-isl-entity ((isl-entity isl-entity) stream)
  (declare (ignore isl-entity stream))
  (values))

(defmacro define-isl-entity
    (name &key (superclasses '(isl-entity))
            (free (alexandria:required-argument :free))
            copy)
  (let ((make (make-isl-sym "%MAKE-" name)))
    `(progn
       (defclass ,name ,superclasses
         ())
       (defun ,make (handle)
           (make-instance ',name :handle handle))
       (defmethod initialize-instance :after ((,name ,name) &key &allow-other-keys)
         (let ((handle (handle-of ,name)))
           (trivial-garbage:finalize ,name (lambda () (,free handle)))))
       ,@(when copy
           `((defmethod copy-isl-entity ((,name ,name))
               (,make (,copy (handle-of ,name)))))))))
