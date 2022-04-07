(defsystem :cl-isl-test-suite
  :description "The CL-ISL Test Suite."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "1.0"
  :license "MIT"
  :depends-on ("cl-isl")
  :serial t
  :components
  ((:file "packages")))
