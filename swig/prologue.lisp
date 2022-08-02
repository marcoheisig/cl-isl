(in-package #:cl-isl)

(cffi:define-foreign-library libisl
  (:darwin (:or "libisl.dylib" "libisl.22.dylib"))
  (:unix (:or "libisl.so" "libisl.so.22"))
  (t (:default "libisl")))

(cffi:use-foreign-library libisl)
