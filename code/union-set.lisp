(in-package :cl-isl)

(defgeneric union-set-from-basic-set ()
  (%isl-union-set-from-basic-set basic-set))

(define-isl-entity union-set :free %isl-union-set-free)
