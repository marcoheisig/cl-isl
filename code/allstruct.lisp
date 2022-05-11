(in-package :cl-isl)

(define-isl-object context :free %isl-ctx-free)

(define-isl-object value :free %isl-val-free :copy %isl-val-copy)


(define-isl-object identifier :free %isl-id-free :copy %isl-id-copy)


(define-isl-object space :free %isl-space-free :copy %isl-space-copy)


(define-isl-object local-space :free %isl-local-space-free :copy %isl-local-space-copy)


(define-isl-object constraint
  :free nil ;; We'll write only function that takes constraint, hopefully no memory will leak
  :copy (lambda (c) (break "You tried to copy a constraint, you can't!!!!")))


(define-isl-object point :free %isl-point-free :copy %isl-point-copy)


(define-isl-object basic-set :free %isl-basic-set-free :copy %isl-basic-set-copy)

(define-isl-object basic-map :free %isl-basic-map-free :copy %isl-basic-map-copy)


(define-isl-object set :free %isl-set-free)

(define-isl-object union-set :free %isl-union-set-free :copy %isl-union-set-copy)

(define-isl-object union-map :free %isl-union-map-free :copy %isl-union-map-copy)


(define-isl-object ast-expr
  :free %isl-ast-expr-free
  :copy %isl-ast-expr-copy
  :abstract t)

;;todo all the generics ?
(define-isl-object ast-node
  :free %isl-ast-node-free
  :copy %isl-ast-node-copy)


(define-isl-object ast-node-list
  :free %isl-ast-node-list-free
  :copy %isl-ast-node-list-copy)


(define-isl-object aff
  :free %isl-aff-free
  :copy %isl-aff-copy)

