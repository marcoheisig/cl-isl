(in-package :cl-isl)

(define-isl-entity value :free %isl-val-free :copy %isl-val-copy)

(defmethod isl-entity-plist ((value value))
  (list :str (%isl-val-to-str (isl-entity-handle value))))

(defmethod print-object ((value value) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-val-to-str (value-handle value)) stream)))

(define-isl-function value-context %isl-val-get-ctx
    (:give context)
    (:keep value))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give value)
                  (:parm context *context*))))
  (def value-zero %isl-val-zero)
  (def value-one %isl-val-one)
  (def value-minus-one %isl-val-negone)
  (def value-nan %isl-val-nan)
  (def value-positive-infinity %isl-val-infty)
  (def value-negative-infinity %isl-val-neginfty))

;; TODO Handle bignums
(define-isl-function value-from-integer %isl-val-int-from-si
    (:give value)
    (:parm context *context*)
    (:keep (signed-byte 64)))

(define-isl-function integer-from-value %isl-val-get-num-si
    (:give (signed-byte 64))
    (:keep value))

(define-isl-function value-sign %isl-val-sgn
    (:give (integer -1 1))
    (:keep value))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give boolean)
                  (:keep value))))
  (def value-zerop %isl-val-is-zero)
  (def value-onep %isl-val-is-one)
  (def value-minus-one-p %isl-val-is-negone)
  (def value-not-minusp %isl-val-is-nonneg)
  (def value-not-plusp %isl-val-is-nonpos)
  (def value-plusp %isl-val-is-pos)
  (def value-minusp %isl-val-is-neg)
  (def value-integerp %isl-val-is-int)
  (def value-rationalp %isl-val-is-rat)
  (def value-nan-p %isl-val-is-nan)
  (def value-positive-infinity-p %isl-val-infty)
  (def value-negative-infinity-p %isl-val-neginfty))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give boolean)
                  (:keep value)
                  (:keep value))))
  (def value<  %isl-val-lt)
  (def value<= %isl-val-le)
  (def value>  %isl-val-gt)
  (def value>= %isl-val-ge)
  (def value= %isl-val-eq)
  (def value/= %isl-val-ne)
  (def value-abs= %isl-val-abs-eq))

(define-isl-function value-divisible-by %isl-val-is-divisible-by
    (:give boolean)
    (:keep value)
    (:keep value))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give value)
                  (:take value))))
  (def value-abs %isl-val-abs)
  (def value-neg %isl-val-neg)
  (def value-floor %isl-val-floor)
  (def value-ceiling %isl-val-ceil)
  (def value-truncate %isl-val-trunc)
  (def value-inverse %isl-val-inv)
  (def value-expt2 %isl-val-pow2))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give value)
                  (:take value)
                  (:take value))))
  (def two-arg-value-min %isl-val-min)
  (def two-arg-value-max %isl-val-max)
  (def two-arg-value+ %isl-val-add)
  (def two-arg-value- %isl-val-sub)
  (def two-arg-value-mul %isl-val-mul)
  (def two-arg-value-div %isl-val-div)
  (def value-mod %isl-val-mod)
  (def value-gcd %isl-val-gcd))

(define-isl-function value-gcdext %isl-val-gcdext
    (:give value)
    (:take value a)
    (:take value b)
    (:give value x)
    (:give value y))
