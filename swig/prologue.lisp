(in-package #:cl-isl)

(cffi:define-foreign-library libisl
  (:unix (:or "libisl.so" "libisl.so.22"))
  (t (:default "libisl")))

(cffi:use-foreign-library libisl)
