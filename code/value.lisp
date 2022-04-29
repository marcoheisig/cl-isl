(in-package :cl-isl)

(define-isl-entity value :free %isl-val-free :copy %isl-val-copy)

(defmethod write-isl-entity ((value value) stream)
  (write-string (%isl-val-to-str (isl-entity-handle value)) stream))

(define-isl-function value-context %isl-val-get-ctx
    (:result-type context)
    (value :keep))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:result-type value :ctx t))))
  (def value-zero %isl-val-zero)
  (def value-one %isl-val-one)
  (def value-minus-one %isl-val-negone)
  (def value-nan %isl-val-nan)
  (def value-positive-infinity %isl-val-infty)
  (def value-negative-infinity %isl-val-neginfty))

;; TODO Handle bignums
(define-isl-function value-from-integer %isl-val-int-from-si
    (:result-type value :ctx t)
    ((signed-byte 64)))

(define-isl-function integer-from-value %isl-val-get-num-si
    (:result-type (signed-byte 64))
    (value :keep))

(define-isl-function value-sign %isl-val-sgn
    (:result-type (integer -1 1))
    (value :keep))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:result-type boolean)
                  (value :keep))))
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
                  (:result-type boolean)
                  (value :keep)
                  (value :keep))))
  (def value<  %isl-val-lt)
  (def value<= %isl-val-le)
  (def value>  %isl-val-gt)
  (def value>= %isl-val-ge)
  (def value= %isl-val-eq)
  (def value/= %isl-val-ne)
  (def value-abs= %isl-val-abs-eq))

(define-isl-function value-divisible-by %isl-val-is-divisible-by
    (:result-type boolean)
    (value :keep)
    (value :keep))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:result-type value)
                  (value :take))))
  (def value-abs %isl-val-abs)
  (def value-neg %isl-val-neg)
  (def value-floor %isl-val-floor)
  (def value-ceiling %isl-val-ceil)
  (def value-truncate %isl-val-trunc)
  (def value-inverse %isl-val-inv)
  (def value-expt2 %isl-val-pow2))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:result-type value)
                  (value :take)
                  (value :take))))
  (def two-arg-value-min %isl-val-min)
  (def two-arg-value-max %isl-val-max)
  (def two-arg-value+ %isl-val-add)
  (def two-arg-value- %isl-val-sub)
  (def two-arg-value-mul %isl-val-mul)
  (def two-arg-value-div %isl-val-div)
  (def value-mod %isl-val-mod)
  (def value-gcd %isl-val-gcd))

(defun value-gcdext (a b)
  (declare (value a b))
  (cffi:with-foreign-objects ((x :pointer) (y :pointer))
    (values
     (%make-value (%isl-val-gcdext (isl-entity-handle a) (isl-entity-handle b) x y))
     (%make-value (cffi:mem-ref x :pointer))
     (%make-value (cffi:mem-ref y :pointer)))))
