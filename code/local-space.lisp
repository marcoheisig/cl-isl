(in-package :cl-isl)

(define-isl-object local-space
  :free %isl-local-space-free
  :copy %isl-local-space-copy
  :from-str nil)

(defmethod print-object ((local-space local-space) stream)
  (print-unreadable-object (local-space stream :type t)
    (write-string
     (%isl-space-to-str
      (space-handle (local-space-space local-space))) stream)))

(define-isl-function local-space-from-space %isl-local-space-from-space
  (:give local-space)
  (:take space))

(define-isl-function local-space-space %isl-local-space-get-space
  (:give space)
  (:keep local-space))

