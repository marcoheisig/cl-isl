(in-package :cl-isl)

(define-isl-object space
  :free %isl-space-free
  :copy %isl-space-copy
  :from-str nil)

(defmethod print-object ((value space) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-space-to-str (space-handle value)) stream)))

(define-isl-function space-unit %isl-space-unit
  (:give space)
  (:keep context))

(define-isl-function space-alloc %isl-space-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam)
  (:keep integer n_in)
  (:keep integer n_out))

(define-isl-function space-params-alloc %isl-space-params-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam))

(define-isl-function space-set-alloc %isl-space-set-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam)
  (:keep integer dim))
