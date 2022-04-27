(in-package #:cl-isl)

(defgeneric make-empty-set (space))

(defgeneric make-empty-union-set (space))

(define-isl-entity set :free %isl-set-free)
