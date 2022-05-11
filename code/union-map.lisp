(in-package :cl-isl)

(define-isl-object union-map
  :free %isl-union-map-free
  :copy %isl-union-map-copy
  :list-type union-map-list)

(defmethod print-object ((value union-map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-map-to-str (union-map-handle value)) stream)))

(define-isl-function union-map-empty %isl-union-map-empty
  (:give union-map)
  (:take space))

(define-isl-function union-map-universe %isl-union-map-universe
  (:give union-map)
  (:take space))

(define-isl-function union-map-from-basic-map %isl-union-map-from-basic-map
  (:give union-map)
  (:take basic-map))

(define-isl-function union-map-from-domain-and-range %isl-union-map-from-domain-and-range
  (:give union-map)
  (:take union-set)
  (:take union-set))

(define-isl-function union-map-apply-range %isl-union-map-apply-range
  (:give union-map)
  (:take union-map)
  (:take union-map))

(define-isl-function union-set-identity %isl-union-set-identity
  (:give union-map)
  (:take union-set))

(define-isl-function union-map-union %isl-union-map-union
  (:give union-map)
  (:take union-map)
  (:take union-map))

(define-isl-function union-map-intersect-domain %isl-union-map-intersect-domain
  (:give union-map)
  (:take union-map)
  (:take union-set))

(define-isl-function union-map-from-str %isl-union-map-read-from-str
  (:give union-map)
  (:keep context)
  (:keep string))

