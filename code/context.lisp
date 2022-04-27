(in-package #:cl-isl)

(defgeneric make-context ())

(define-isl-entity context :free %isl-ctx-free)

(defmethod make-context ()
  (let ((handle (%isl-ctx-alloc)))
    (%isl-options-set-on-error handle +isl-on-error-continue+)
    (%make-context handle)))

(defvar *context* (make-context))
