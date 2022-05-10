(in-package #:cl-isl)

(defgeneric make-empty-set (space))

(define-isl-entity set :free %isl-set-free)
