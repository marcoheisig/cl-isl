(in-package :cl-isl)

(define-isl-entity ast-node
  :free %isl-ast-node-free
  :copy %isl-ast-node-copy)

(define-isl-entity for-node
  :superclass ast-node
  :free %isl-ast-node-free
  :copy %isl-ast-node-copy)

(defmethod print-object ((ast ast-node) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-ast-node-to-str (value-handle value)) stream)))

(deftype ast-expr-type ()
  `(member ,@(remove :isl-ast-expr-error (cffi:foreign-enum-keyword-list 'isl-ast-expr-type))))

(define-isl-function node-get-type %isl-ast-node-get-type
  (:give (member . #.(cffi:foreign-enum-keyword-list 'isl-ast-expr-type)))
  (:keep ast-node))

;; FOR NODE
(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give ast-expr)
                (:keep ast-node))))
  (def for-node-get-iterator %isl-ast-node-for-get-iterator)
  (def for-node-get-init %isl-ast-node-for-get-init)
  (def for-node-get-cond %isl-ast-node-for-get-cond)
  (def for-node-get-inc %isl-ast-node-for-get-inc)
  (def for-node-get-body %isl-ast-node-for-get-body))

;; IF NODE
(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give ast-expr)
                (:keep ast-node))))
  (def if-node-get-cond %isl-ast-node-if-get-cond)
  (def if-node-get-then %isl-ast-node-if-get-then)
  (def if-node-get-else %isl-ast-node-if-get-else))

(define-isl-function if-node-hash-else %isl-ast-node-if-has-else
  (:give bool)
  (:keep ast-node))

;; The rest - todo
