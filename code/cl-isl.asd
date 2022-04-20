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
   (:cffi-grovel-file "grovel")
   (:file "isl")
   (:file "code")
   (:file "polyhedral")))
