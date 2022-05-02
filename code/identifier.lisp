(in-package #:cl-isl)

(define-isl-entity identifier :free %isl-id-free :copy %isl-id-copy)

(defun make-identifier (name)
  (declare (symbol name))
  (%make-identifier
   (cffi:with-foreign-string (char* (string-from-symbol name))
     (%isl-id-alloc (context-handle *context*) char* (cffi:null-pointer)))))

(defun identifier-name (identifier)
  (declare (identifier identifier))
  (let* ((handle (identifier-handle identifier))
         (char* (%isl-id-get-name handle)))
    (read-from-string
     (cffi:foreign-string-to-lisp char*))))

(define-isl-function identifier-context %isl-id-get-ctx
    (:give context)
    (:keep identifier))

(defmethod isl-entity-plist ((identifier identifier))
  (list :name (identifier-name identifier)))
