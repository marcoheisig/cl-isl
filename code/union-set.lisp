(in-package :cl-isl)

(define-isl-object union-set
  :free %isl-union-set-free
  :copy %isl-union-set-copy
  :list-type union-set-list)

(defmethod print-object ((value union-set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-set-to-str (union-set-handle value)) stream)))

;; Creation

(define-isl-function union-set-empty %isl-union-set-empty
  (:give union-set)
  (:take space))

(define-isl-function union-set-universe %isl-union-set-universe
  (:give union-set)
  (:take space))

;; Conversion

(define-isl-function basic-set-union-set %isl-union-set-from-basic-set
  (:give union-set)
  (:take basic-set))

(define-isl-function set-union-set %isl-set-from-basic-set
  (:give union-set)
  (:take set))

;; Todo check which operation to include

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
