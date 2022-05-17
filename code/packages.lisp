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
   ;; Space
   #:space
   #:spacep
   #:space-unit
   #:space-alloc
   #:space-params-alloc
   #:space-set-alloc
   ;; Local Space
   #:local-space
   #:local-space-p
   #:local-space-from-space
   #:local-space-space
   ;; Set
   #:set
   #:setp
   #:set-from-str
   #:set-empty
   #:set-universe
   ;; Basic set
   #:basic-set-set
   #:basic-set
   #:basic-set-p
   #:basic-set-from-str
   #:basic-set-empty
   #:basic-set-universe
   #:basic-set-add-constraint
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
   #:union-set-is-equal
   #:union-set-<=
   #:union-set-<
   ;;#:union-set->=
   ;;#:union-set->
   ;; Map
   #:map
   #:basic-map
   #:union-map
   ;; Ast
   #:ast-node
   #:ast-build
   #:schedule
   ))
