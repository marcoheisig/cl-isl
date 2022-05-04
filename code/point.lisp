(in-package :cl-isl)

(define-isl-entity point :free %isl-point-free :copy %isl-point-copy)

(defmethod isl-entity-plist ((value point))
  (list :str (%isl-point-to-str (isl-entity-handle value))))

(defmethod print-object ((value point) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-point-to-str (point-handle value)) stream)))

(define-isl-function point-zero %isl-point-zero
  (:give point)
  (:take isl-space))

(deftype dim-type ()
  `(member ,@(remove nil (cffi:foreign-enum-keyword-list 'isl-dim-type))))

(defun point-set-coord-2 (p type a b)
  (%make-point
   (%isl-point-set-coordinate-val
    (point-handle (copy p))
    type
    a
    (%isl-val-int-from-si (context-handle *context*) b))))
