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
   ;; Identifier
   #:identifier
   #:identifierp
   #:make-identifier
   #:identifier-name
   #:identifier-context
   ;; Space
   ;; Set
   #:set
   #:basic-set
   #:union-set
   ;; Map
   #:map
   #:basic-map
   #:union-map
   ;; Ast
   #:ast-node
   #:ast-build
   #:schedule
   ))
