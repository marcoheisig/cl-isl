(in-package :cl-isl)

(define-isl-entity ast-expr
  :free %isl-ast-expr-free
  :copy %isl-ast-expr-copy
  :abstract t)

(define-isl-entity op-expr
  :superclass ast-expr
  :abstract t)

(define-isl-entity id-expr
  :superclass ast-expr)

(define-isl-entity int-expr
  :superclass ast-expr)

(defun %make-ast-expr (handle)
  (ecase (%isl-ast-expr-get-type handle)
    (:isl-ast-expr-error (isl-error))
    (:isl-ast-expr-op (%make-op-expr handle))
    (:isl-ast-expr-id (%make-id-expr handle))
    (:isl-ast-expr-int (%make-int-expr handle))))

(define-isl-entity op-and
  :superclass op-expr)

(define-isl-entity op-and-then
  :superclass op-expr)

(define-isl-entity op-or
  :superclass op-expr)

(define-isl-entity op-or-else
  :superclass op-expr)

(define-isl-entity op-call
  :superclass op-expr)

(define-isl-entity op-le
  :superclass op-expr)

(defun %make-op-expr (handle)
  (ecase (%isl-ast-expr-op-get-type handle)
    (:isl-ast-expr-op-error (isl-error))
    (:isl-ast-expr-op-and (%make-op-and handle))
    (:isl-ast-expr-op-and-then (%make-op-and-then handle))
    (:isl-ast-expr-op-or (%make-op-or handle))
    (:isl-ast-expr-op-or-else (%make-op-or-else handle))
    (:isl-ast-expr-op-le (%make-op-le handle))
    (:isl-ast-expr-op-call (%make-op-call handle))
    ;; TODO
    ))

(defmethod print-object ((ast-expr ast-expr) stream)
  (print-unreadable-object (ast-expr stream :type t)
    (write-string (%isl-ast-expr-to-str (ast-expr-handle ast-expr)) stream)))

;; Expr op

(define-isl-function op-expr-get-n-arg %isl-ast-expr-op-get-n-arg
  (:give (unsigned-byte 32)) ;int
  (:keep ast-expr))

(define-isl-function op-expr-get-op-arg %isl-ast-expr-get-op-arg
  (:give ast-expr)
  (:keep ast-expr)
  (:keep (unsigned-byte 32)))

;;Returns a list of every son of the ast.
(defun op-expr-get-list-args (ast)
  ;; assert type ast-exp op
  (let ((n (op-expr-get-n-arg ast)))
    (loop for i below n do
      (op-expr-get-op-arg ast i))))


;; ID

(define-isl-function id-expr-get-id %isl-ast-expr-get-id
  (:give identifier)
  (:keep id-expr))

;; INT

(define-isl-function int-expr-get-value %isl-ast-expr-get-val
  (:give value)
  (:keep int-expr))

