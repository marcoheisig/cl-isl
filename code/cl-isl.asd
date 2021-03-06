(defsystem :cl-isl
  :description "A Common Lisp Interface for the Integer Set Library (ISL)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "1.0"
  :license "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("alexandria" "cffi" "trivial-garbage")
  :in-order-to ((test-op (test-op "cl-isl-test-suite")))
  :serial t
  :components
  ((:file "packages")
   (:file "swig-lispify")
   (:cffi-grovel-file "grovel")
   (:file "swig-interface")
   (:file "types")
   (:file "utilities")
   (:file "isl-object")
   (:file "isl-function")
   (:file "context")
   (:file "value")
   (:file "identifier")
   (:file "space")
   (:file "local-space")
   (:file "constraint")
   (:file "affine-expression")
   (:file "basic-set")
   (:file "set")
   (:file "union-set")
   (:file "basic-map")
   (:file "map")
   (:file "union-map")
   (:file "ast-expr")
   (:file "ast-node")
   (:file "ast-build")
   (:file "schedule-constraints")
   (:file "schedule")
   (:file "ast-generation")))
