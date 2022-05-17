(in-package :cl-isl)

(define-isl-object union-set
  :free %isl-union-set-free
  :copy %isl-union-set-copy
  :list-type union-set-list)

(defmethod print-object ((value union-set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-set-to-str (union-set-handle value)) stream)))

;; Creation

(define-isl-function union-set-empty %isl-union-set-empty
  (:give union-set)
  (:take space))

(define-isl-function union-set-universe %isl-union-set-universe
  (:give union-set)
  (:take space))

;; Conversion

(define-isl-function basic-set-union-set %isl-union-set-from-basic-set
  (:give union-set)
  (:take basic-set))

(define-isl-function set-union-set %isl-set-from-basic-set
  (:give union-set)
  (:take set))

;; Operations

;; (set, set) -> set

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give union-set)
                (:take union-set)
                (:take union-set))))
  (def union-set-intersect %isl-union-set-intersect)
  (def union-set-union %isl-union-set-union)
  (def union-set-subtract %isl-union-set-subtract)
  (def union-set-product %isl-union-set-product)
  (def union-set-lex-lt-union-set %isl-union-set-lex-lt-union-set)
  (def union-set-lex-le-union-set %isl-union-set-lex-le-union-set)
  (def union-set-lex-gt-union-set %isl-union-set-lex-gt-union-set)
  (def union-set-lex-ge-union-set %isl-union-set-lex-ge-union-set))



;; union is +
;; intersect is *

;; works for map too, * too
;; generic thing? or union-map-* and union-set-*

;; (set, set) -> bool
;; Todo check what is returned (the isl boolean or the lisp boolean)

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give boolean)
                (:take union-set)
                (:take union-set))))
  (def union-set-is-equal %isl-union-set-is-equal)
  (def union-set-<= %isl-union-set-is-subset)
  (def union-set-< %isl-union-set-is-strict-subset)
  ;;(def union-set->= (lambda (a b) (%isl-union-set-is-subset a b)))
  ;;(def union-set-> nil)
  )
