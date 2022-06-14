(in-package :cl-isl)

(define-isl-object basic-set
  :free %isl-basic-set-free
  :copy %isl-basic-set-copy
  :list-type basic-set-list
  :from-str t)

(defmethod print-object ((value basic-set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-basic-set-to-str (basic-set-handle value)) stream)))

;; Creation

(define-isl-function basic-set-empty %isl-basic-set-empty
  (:give basic-set)
  (:take space))

(define-isl-function basic-set-universe %isl-basic-set-universe
  (:give basic-set)
  (:take space))

;; todo multi aff type

(define-isl-function basic-set-from-multi-aff %isl-basic-set-from-multi-aff
  (:give basic-set)
  (:take affine))

(define-isl-function basic-set-intersect %isl-basic-set-intersect
  (:give basic-set)
  (:take basic-set)
  (:take basic-set))

(define-isl-function affine-basic-set %isl-aff-eq-basic-set
  (:give basic-set)
  (:take affine)
  (:take affine))

;; Constraints

(define-isl-function basic-set-add-constraint %isl-basic-set-add-constraint
  (:give basic-set)
  (:take basic-set)
  (:take constraint))
