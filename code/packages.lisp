(cl:in-package #:cl-user)

(defpackage #:cl-isl
  (:nicknames #:isl)
  (:use #:common-lisp)
  (:shadow #:set #:map #:space)
  (:export
   #:copy
   ;; Context
   #:context
   #:contextp
   #:*context*
   #:make-context
   ;; Identifier
   #:identifier
   #:identifierp
   #:make-identifier
   #:make-gensym-identifier
   #:identifier-name
   #:identifier-context
   ;; Value
   #:value
   #:valuep
   #:value-context
   #:value-zero
   #:value-one
   #:value-minus-one
   #:value-nan
   #:value-positive-infinity
   #:value-negative-infinity
   #:value-from-integer
   #:integer-from-value
   #:value-sign
   #:value-zerop
   #:value-onep
   #:value-minus-one-p
   #:value-not-minusp
   #:value-not-plusp
   #:value-plusp
   #:value-minusp
   #:value-integerp
   #:value-rationalp
   #:value-nan-p
   #:value-positive-infinity-p
   #:value-negative-infinity-p
   #:value-<=
   #:value-<
   #:value->=
   #:value->
   #:value=
   #:value/=
   #:value-abs
   #:value-neg
   #:value-floor
   #:value-ceiling
   #:value-truncate
   #:value-inverse
   #:value-expt2
   #:value-min
   #:value-max
   #:value+
   #:value-
   #:value-mul
   #:value-div
   #:value-mod
   #:value-gcd
   #:value-gcdext
   #:value-object
   ;; Space
   #:space
   #:spacep
   #:space-add-param-id
   #:create-space-params
   #:create-space-set
   #:create-space-map
   ;; Local Space
   #:local-space
   #:local-space-p
   #:local-space-from-space
   #:local-space-space
   ;; Constraint
   #:equality-constraint
   #:equality-constraint-p
   #:make-equality-constraint
   #:equality-constraint-set-constant
   #:equality-constraint-set-coefficient
   #:inequality-constraint
   #:inequality-constraint-p
   #:make-inequality-constraint
   #:inequality-constraint-set-constant
   #:inequality-constraint-set-coefficient
   ;; Affine expression
   #:affine-expression
   #:affine-expression-p
   #:affine-expression-from-str
   #:create-empty-affine
   #:create-val-affine
   #:create-var-affine
   #:affine-add
   #:affine-sub
   #:affine-mul
   #:affine-div
   ;; Set
   #:set
   #:setp
   #:set-from-str
   #:set-empty
   #:set-universe
   ;; Basic set
   #:basic-set
   #:basic-set-p
   #:basic-set-from-str
   #:basic-set-empty
   #:basic-set-universe
   #:basic-set-intersect
   #:basic-set-set
   #:basic-set-add-constraint
   #:basic-set-apply
   ;; Union set
   #:union-set
   #:union-set-p
   #:union-set-from-str
   #:union-set-empty
   #:union-set-universe
   #:basic-set-union-set
   #:set-union-set
   #:union-set-intersect
   #:union-set-union
   #:union-set-subtract
   #:union-set-product
   #:union-set-lex-lt-union-set
   #:union-set-lex-le-union-set
   #:union-set-lex-gt-union-set
   #:union-set-lex-ge-union-set
   #:union-set-equalp
   #:union-set-subsetp
   #:union-set-strict-subset-p
   #:union-set-intersect-params
   ;; Map
   #:map
   #:mapp
   #:map-from-str
   #:map-empty
   #:map-universe
   ;; Basic map
   #:basic-map
   #:basic-map-p
   #:basic-map-from-str
   #:basic-map-empty
   #:basic-map-universe
   #:basic-map-from-affine
   #:basic-map-map
   #:basic-map-intersect
   #:basic-map-add-constraint
   #:basic-map-insert-dimension
   ;; Union map
   #:union-map
   #:union-map-p
   #:union-map-from-str
   #:union-map-empty
   #:union-map-universe
   #:basic-map-union-map
   #:map-union-map
   #:union-map-reverse
   #:union-map-intersect
   #:union-map-union
   #:union-map-subtract
   #:union-map-product
   #:union-map-lex-lt-union-map
   #:union-map-lex-le-union-map
   #:union-map-lex-gt-union-map
   #:union-map-lex-ge-union-map
   #:union-map-equalp
   #:union-map-subsetp
   #:union-map-strict-subset-p
   #:union-map-domain
   #:union-map-range
   #:union-map-from-domain-and-range
   #:union-set-identity
   #:union-map-intersect-params
   #:union-map-intersect-domain
   #:union-map-intersect-range
   #:union-map-subtract-domain
   #:union-map-subtract-range
   #:union-set-apply
   #:union-map-apply-range
   #:union-map-apply-domain
   ;; Ast expr
   #:ast-expr
   #:ast-expr-p
   #:ast-expr-equal-p
   #:create-ast-expr-from-val
   #:create-ast-expr-from-add
   ;; -- Op expr
   #:op-expr
   #:op-expr-p
   #:op-and
   #:op-and-then
   #:op-or
   #:op-or-else
   #:op-max
   #:op-min
   #:op-minus
   #:op-add
   #:op-sub
   #:op-mul
   #:op-div
   #:op-fdiv-q
   #:op-pdiv-q
   #:op-pdiv-r
   #:op-zdiv-r
   #:op-cond
   #:op-select
   #:op-eq
   #:op-le
   #:op-lt
   #:op-ge
   #:op-gt
   #:op-call
   #:op-access
   #:op-member
   #:op-address-of
   #:op-and-p
   #:op-and-then-p
   #:op-or-p
   #:op-or-else-p
   #:op-max-p
   #:op-min-p
   #:op-minus-p
   #:op-add-p
   #:op-sub-p
   #:op-mul-p
   #:op-div-p
   #:op-fdiv-q-p
   #:op-pdiv-q-p
   #:op-pdiv-r-p
   #:op-zdiv-r-p
   #:op-cond-p
   #:op-select-p
   #:op-eq-p
   #:op-le-p
   #:op-lt-p
   #:op-ge-p
   #:op-gt-p
   #:op-call-p
   #:op-access-p
   #:op-member-p
   #:op-address-of-p
   #:op-expr-get-n-arg
   #:op-expr-get-op-arg
   #:op-expr-get-list-args
   #:op-expr-get-operator
   ;; -- Id expr
   #:id-expr
   #:id-expr-p
   #:id-expr-get-id
   ;; -- Int expr
   #:int-expr
   #:int-expr-p
   #:int-expr-get-value
   ;; Ast node
   #:ast-node
   #:ast-node-p
   #:pretty-print-node
   #:ast-node-list-elements ; todo export every list object
   ;; -- For node
   #:for-node
   #:for-node-p
   #:for-node-get-iterator
   #:for-node-get-init
   #:for-node-get-cond
   #:for-node-get-inc
   #:for-node-get-body
   ;; -- If node
   #:if-node
   #:if-node-p
   #:if-node-get-cond
   #:if-node-get-then
   #:if-node-get-else
   #:if-node-has-else
   ;; -- Block node
   #:block-node
   #:block-node-p
   #:block-node-getlist
   ;; -- Mark node
   #:mark-node
   #:mark-node-p
   ;; -- User node
   #:user-node
   #:user-node-p
   #:user-node-get-expr
   ;; Ast build
   #:ast-build
   #:ast-build-p
   #:create-ast-build
   #:generate-optimized-ast
   ;; Schedule
   #:schedule
   #:schedulep
   #:schedule-constraints-compute-schedule
   ;; Schedule constraints
   #:schedule-constraints
   #:schedule-constraints-p
   #:schedule-constraints-set-context
   #:schedule-constraints-set-validity
   #:schedule-constraints-set-coincidence
   #:schedule-constraints-set-proximity
   #:schedule-constraints-set-conditional-validity
   ))
