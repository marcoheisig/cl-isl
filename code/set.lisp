(in-package #:cl-isl)

(defgeneric make-empty-set (space))

(define-isl-object set :free %isl-set-free)
