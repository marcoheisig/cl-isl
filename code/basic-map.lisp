(in-package :cl-isl)

(define-isl-object basic-map
  :free %isl-basic-map-free
  :copy %isl-basic-map-copy
  :list-type basic-map-list
  :from-str t)

(defmethod print-object ((value basic-map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-basic-map-to-str (basic-map-handle value)) stream)))

;; Creation

(define-isl-function basic-map-empty %isl-basic-map-empty
   (:give basic-map)
   (:take space))

(define-isl-function basic-map-universe %isl-basic-map-universe
  (:give basic-map)
  (:take space))

;; Constraints

(define-isl-function basic-map-add-constraint %isl-basic-map-add-constraint
  (:give basic-map)
  (:take basic-map)
  (:take constraint))



