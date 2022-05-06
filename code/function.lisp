;; This file defines structure that represent functions in isl
;; No link whatsoever with isl-function.lisp

;; For now, quasi affine expression only
;; Was a test, not used

(in-package :cl-isl)

(define-isl-entity aff
  :free %isl-aff-free
  :copy %isl-aff-copy)

(defmethod isl-entity-plist ((value aff))
  (list :str (%isl-aff-to-str (isl-entity-handle value))))

(defmethod print-object ((value aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-aff-to-str (aff-handle value)) stream)))

(define-isl-function aff-val-on-domain-space %isl-aff-val-on-domain-space
  (:give aff)
  (:take isl-space)
  (:take value))

(define-isl-function aff-le-basic-set %isl-aff-le-basic-set
  (:give basic-set)
  (:take aff)
  (:take aff))

(define-isl-function aff-ge-basic-set %isl-aff-ge-basic-set
  (:give basic-set)
  (:take aff)
  (:take aff))
