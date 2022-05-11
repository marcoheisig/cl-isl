(in-package #:cl-isl)

(define-isl-object map
  :free %isl-map-free
  :copy %isl-map-copy
  :list-type map-list)

(defmethod print-object ((value map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-map-to-str (map-handle value)) stream)))
