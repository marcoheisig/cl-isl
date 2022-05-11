(in-package :cl-isl)

(define-isl-entity context :free %isl-ctx-free)

(define-isl-entity value :free %isl-val-free :copy %isl-val-copy)


(define-isl-entity identifier :free %isl-id-free :copy %isl-id-copy)


(define-isl-entity isl-space :free %isl-space-free :copy %isl-space-copy)


(define-isl-entity local-space :free %isl-local-space-free :copy %isl-local-space-copy)


(define-isl-entity constraint
  :free nil ;; We'll write only function that takes constraint, hopefully no memory will leak
  :copy (lambda (c) (break "You tried to copy a constraint, you can't!!!!")))


(define-isl-entity point :free %isl-point-free :copy %isl-point-copy)


(define-isl-entity basic-set :free %isl-basic-set-free :copy %isl-basic-set-copy)

(define-isl-entity basic-map :free %isl-basic-map-free :copy %isl-basic-map-copy)


(define-isl-entity set :free %isl-set-free)

(define-isl-entity union-set :free %isl-union-set-free :copy %isl-union-set-copy)

(define-isl-entity union-map :free %isl-union-map-free :copy %isl-union-map-copy)


(define-isl-entity ast-expr
  :free %isl-ast-expr-free
  :copy %isl-ast-expr-copy
  :abstract t)

;;todo all the generics ?
(define-isl-entity ast-node
  :free %isl-ast-node-free
  :copy %isl-ast-node-copy)


(define-isl-entity ast-node-list
  :free %isl-ast-node-list-free
  :copy %isl-ast-node-list-copy)


(define-isl-entity aff
  :free %isl-aff-free
  :copy %isl-aff-copy)

