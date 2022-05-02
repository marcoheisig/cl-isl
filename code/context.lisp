(in-package #:cl-isl)

(define-isl-entity context :free %isl-ctx-free)

(defmethod isl-entity-plist ((context context))
  (list :handle (context-handle context)))

(defun make-context ()
  (let ((handle (%isl-ctx-alloc)))
    (%isl-options-set-on-error handle +isl-on-error-continue+)
    (%make-context handle)))

(defvar *context* (make-context))
