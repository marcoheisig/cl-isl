(in-package #:cl-isl)

(define-isl-object identifier :free %isl-id-free :copy %isl-id-copy)

(defmethod print-object ((identifier identifier) stream)
  (print-unreadable-object (identifier stream :type t)
    (write-string (%isl-id-to-str (identifier-handle identifier)) stream)))

(defun make-identifier (name)
  (declare (symbol name))
  (%make-identifier
   (cffi:with-foreign-string (char* (string-from-symbol name))
     (%isl-id-alloc (context-handle *context*) char* (cffi:null-pointer)))))

(defun identifier-name (identifier)
  (declare (identifier identifier))
  (let* ((handle (identifier-handle identifier))
         (char* (%isl-id-get-name handle)))
    (values
     (read-from-string
      (cffi:foreign-string-to-lisp char*)))))

(define-isl-function identifier-context %isl-id-get-ctx
    (:give context)
    (:keep identifier))
