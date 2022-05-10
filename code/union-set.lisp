(in-package :cl-isl)

(define-isl-entity union-set :free %isl-union-set-free :copy %isl-union-set-copy)

(defmethod isl-entity-plist ((value union-set))
  (list :str (%isl-union-set-to-str (isl-entity-handle value))))

(defmethod print-object ((value union-set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-set-to-str (union-set-handle value)) stream)))

(define-isl-function union-set-empty %isl-union-set-empty
  (:give union-set)
  (:take isl-space))

(define-isl-function union-set-universe %isl-union-set-universe
  (:give union-set)
  (:take isl-space))

(define-isl-function union-set-from-str %isl-union-set-read-from-str
  (:give union-set)
  (:keep context)
  (:keep string))

(define-isl-function union-set-is-equal %isl-union-set-is-equal
  (:give boolean)
  (:keep union-set)
  (:keep union-set))

(define-isl-function union-set-product %isl-union-set-product
  (:give union-set)
  (:take union-set)
  (:take union-set))

(define-isl-function union-set-union %isl-union-set-union
  (:give union-set)
  (:take union-set)
  (:take union-set))

(define-isl-function union-set-from-basic-set %isl-union-set-from-basic-set
  (:give union-set)
  (:take basic-set))
