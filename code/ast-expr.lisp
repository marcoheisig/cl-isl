(in-package :cl-isl)

(define-isl-object ast-expr
  :free %isl-ast-expr-free
  :copy %isl-ast-expr-copy
  :list-type ast-expr-list
  :abstract t)

(defmethod print-object ((ast-expr ast-expr) stream)
  (print-unreadable-object (ast-expr stream :type t)
    (write-string (%isl-ast-expr-to-str (ast-expr-handle ast-expr)) stream)))

(define-isl-object op-expr
  :superclass ast-expr
  :abstract t)

(define-isl-object id-expr
  :superclass ast-expr)

(define-isl-object int-expr
  :superclass ast-expr)

(defun %make-ast-expr (handle)
  (ecase (%isl-ast-expr-get-type handle)
    (:ast-expr-error (isl-error))
    (:ast-expr-op (%make-op-expr handle))
    (:ast-expr-id (%make-id-expr handle))
    (:ast-expr-int (%make-int-expr handle))))

;; OP

(define-isl-object op-and
  :superclass op-expr)

(define-isl-object op-and-then
  :superclass op-expr)

(define-isl-object op-or
  :superclass op-expr)

(define-isl-object op-or-else
  :superclass op-expr)

(define-isl-object op-max
  :superclass op-expr)

(define-isl-object op-min
  :superclass op-expr)

(define-isl-object op-minus
  :superclass op-expr)

(define-isl-object op-add
  :superclass op-expr)

(define-isl-object op-sub
  :superclass op-expr)

(define-isl-object op-mul
  :superclass op-expr)

(define-isl-object op-div
  :superclass op-expr)

(define-isl-object op-fdiv-q
  :superclass op-expr)

(define-isl-object op-pdiv-q
  :superclass op-expr)

(define-isl-object op-pdiv-r
  :superclass op-expr)

(define-isl-object op-zdiv-q
  :superclass op-expr)

(define-isl-object op-cond
  :superclass op-expr)

(define-isl-object op-select
  :superclass op-expr)

(define-isl-object op-eq
  :superclass op-expr)

(define-isl-object op-le
  :superclass op-expr)

(define-isl-object op-lt
  :superclass op-expr)

(define-isl-object op-ge
  :superclass op-expr)

(define-isl-object op-gt
  :superclass op-expr)

(define-isl-object op-call
  :superclass op-expr)

(define-isl-object op-access
  :superclass op-expr)

(define-isl-object op-member
  :superclass op-expr)

(define-isl-object op-address-of
  :superclass op-expr)

(defun %make-op-expr (handle)
  (ecase (%isl-ast-expr-op-get-type handle)
    (:ast-expr-op-error (isl-error))
    (:ast-expr-op-and (%make-op-and handle))
    (:ast-expr-op-and-then (%make-op-and-then handle))
    (:ast-expr-op-or (%make-op-or handle))
    (:ast-expr-op-or-else (%make-op-or-else handle))
    (:ast-expr-op-max (%make-op-max handle))
    (:ast-expr-op-min (%make-op-min handle))
    (:ast-expr-op-minus (%make-op-minus handle))
    (:ast-expr-op-add (%make-op-add handle))
    (:ast-expr-op-sub (%make-op-sub handle))
    (:ast-expr-op-mul (%make-op-mul handle))
    (:ast-expr-op-div (%make-op-div handle))
    (:ast-expr-op-fdiv-q (%make-op-fdiv-q handle))
    (:ast-expr-op-pdiv-q (%make-op-pdiv-q handle))
    (:ast-expr-op-pdiv-r (%make-op-pdiv-r handle))
    ;;(:ast-expr-op-zdiv-r (%make-op-zdiv-r handle))
    (:ast-expr-op-cond (%make-op-cond handle))
    (:ast-expr-op-select (%make-op-select handle))
    (:ast-expr-op-eq (%make-op-eq handle))
    (:ast-expr-op-le (%make-op-le handle))
    (:ast-expr-op-lt (%make-op-lt handle))
    (:ast-expr-op-ge (%make-op-ge handle))
    (:ast-expr-op-gt (%make-op-gt handle))
    (:ast-expr-op-call (%make-op-call handle))
    (:ast-expr-op-access (%make-op-access handle))
    (:ast-expr-op-member (%make-op-member handle))
    (:ast-expr-op-address-of (%make-op-address-of handle))))

(define-isl-function op-expr-get-n-arg %isl-ast-expr-op-get-n-arg
  (:give (unsigned-byte 32))
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
