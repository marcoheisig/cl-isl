(in-package :cl-isl)

(define-isl-entity isl-space :free %isl-space-free :copy %isl-space-copy)

(defmethod isl-entity-plist ((value isl-space))
  (list :str (%isl-space-to-str (isl-entity-handle value))))

(defmethod print-object ((value isl-space) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-space-to-str (isl-space-handle value)) stream)))

(define-isl-function space-unit %isl-space-unit
  (:give isl-space)
  (:keep context))

(define-isl-function space-alloc %isl-space-alloc
  (:give isl-space)
  (:keep context)
  (:keep integer)
  (:keep integer)
  (:keep integer))

(define-isl-function space-params-alloc %isl-space-params-alloc
  (:give isl-space)
  (:keep context)
  (:keep integer))

(define-isl-function space-set-alloc %isl-space-set-alloc
  (:give isl-space)
  (:keep context)
  (:keep integer)
  (:keep integer))
