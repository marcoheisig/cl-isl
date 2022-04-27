(in-package :cl-isl)

(defgeneric value-from-integer (integer))

(defgeneric integer-from-value (value))

(defgeneric value-sign (value))

(defgeneric value-zerop (value))

(defgeneric value-onep (value))

(defgeneric value-negonep (value))

(defgeneric value-not-minusp (value))

(defgeneric value-not-plusp (value))

(defgeneric value-plusp (value))

(defgeneric value-minusp (value))

(defgeneric value-integerp (value))

(defgeneric value-rationalp (value))

(defgeneric value-nan-p (value))

(defgeneric value-positive-infinity-p (value))

(defgeneric value-negative-infinity-p (value))

(defgeneric value< (value1 value2))

(defgeneric value<= (value1 value2))

(defgeneric value> (value1 value2))

(defgeneric value>= (value1 value2))

(defgeneric value= (value1 value2))

(defgeneric value/= (value1 value2))

(defgeneric value-abs= (value1 value2))

(define-isl-entity value :free %isl-val-free :copy %isl-val-copy)

(defmethod write-isl-entity ((value value) stream)
  (write-string (%isl-val-to-str (handle-of value)) stream))

(defmethod value-from-integer ((integer integer))
  (%make-value
   (%isl-val-int-from-si (handle-of *context*) integer)))

(defmethod integer-from-value ((value value))
  (%isl-val-get-num-si (handle-of value)))

(defmethod value-sign ((value value))
  (%isl-val-sgn (handle-of value)))

(macrolet ((def (name impl)
             `(defmethod ,name ((value value))
                (lispify-isl-bool
                 (,impl (handle-of value))))))
  (def value-zerop %isl-val-is-zero)
  (def value-onep %isl-val-is-one)
  (def value-negonep %isl-val-is-negone)
  (def value-not-minusp %isl-val-is-nonneg)
  (def value-not-plusp %isl-val-is-nonpos)
  (def value-plusp %isl-val-is-pos)
  (def value-minusp %isl-val-is-neg)
  (def value-integerp %isl-val-is-int)
  (def value-rationalp %isl-val-is-rat)
  (def value-nan-p %isl-val-is-nan)
  (def value-positive-infinity-p %isl-val-infty)
  (def value-negative-infinity-p %isl-val-neginfty))

;; TODO
