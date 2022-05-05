(in-package #:cl-isl)

(define-isl-entity basic-set :free %isl-basic-set-free :copy %isl-basic-set-copy)

(defmethod isl-entity-plist ((value basic-set))
  (list :str (%isl-basic-set-to-str (isl-entity-handle value))))

(defmethod print-object ((value basic-set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-basic-set-to-str (basic-set-handle value)) stream)))
