(in-package :cl-isl)

(define-isl-entity ast-expr
  :free %isl-ast-expr-free
  :copy %isl-ast-expr-copy)

(defmethod print-object ((ast ast-expr) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-ast-expr-to-str (value-handle value)) stream)))

(define-isl-function expr-get-type %isl-ast-expr-get-type
  (:give t) ;enum
  (:keep ast-expr))

;; Expr op

(define-isl-function op-expr-get-type %isl-ast-expr-op-ge-type
  (:give t) ;enum
  (:keep ast-expr))

(define-isl-function op-expr-get-n-arg %isl-ast-expr-op-get-n-arg
  (:give t) ;int
  (:keep ast-expr))

(define-isl-function op-expr-get-op-arg %isl-ast-expr-get-op-arg
  (:give ast-expr)
  (:keep ast-expr)
  (:keep t))

;; Returns a list of every son of the ast
(defun op-expr-get-list-args (ast)
  ;; assert type ast-exp op
  (let ((n (op-expr-get-n-arg ast)))
    (loop for i below n do
      (op-expr-get-op-arg ast i))))
