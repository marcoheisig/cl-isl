;; This file defines structure that represent functions in isl
;; No link whatsoever with isl-function.lisp

;; For now, quasi affine expression only
;; Was a test, not used

(in-package :cl-isl)

(define-isl-object affine
  :free %isl-aff-free
  :copy %isl-aff-copy
  :list-type affine-list)

(defmethod print-object ((value affine) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-aff-to-str (affine-handle value)) stream)))

(define-isl-function affine-val-on-domain-space %isl-aff-val-on-domain-space
  (:give affine)
  (:take space)
  (:take value))

(define-isl-function affine-le-basic-set %isl-aff-le-basic-set
  (:give basic-set)
  (:take affine)
  (:take affine))

(define-isl-function affine-ge-basic-set %isl-aff-ge-basic-set
  (:give basic-set)
  (:take affine)
  (:take affine))
