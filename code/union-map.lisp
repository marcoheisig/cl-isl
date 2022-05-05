(in-package :cl-isl)

(define-isl-entity union-map :free %isl-union-map-free :copy %isl-union-map-copy)

(defmethod isl-entity-plist ((value union-map))
  (list :str (%isl-union-map-to-str (isl-entity-handle value))))

(defmethod print-object ((value union-map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-map-to-str (union-map-handle value)) stream)))

(define-isl-function union-map-empty %isl-union-map-empty
  (:give union-map)
  (:take isl-space))

(define-isl-function union-map-from-domain-and-range %isl-union-map-from-domain-and-range
  (:give union-map)
  (:take union-set)
  (:take union-set))

(defun create-single-map (p1 p2)
  (union-map-from-domain-and-range
   (union-set-from-point p1)
   (union-set-from-point p2)))

(when nil
(defun create-single-map (p1 p2)
  (%make-union-map (%isl-union-map-from-domain-and-range
                    (union-set-handle
                     (union-set-from-point p1))
                    (union-set-handle
                     (union-set-from-point p2)))))
)

(define-isl-function union-map-union %isl-union-map-union
  (:give union-map)
  (:take union-map)
  (:take union-map))

(define-isl-function union-map-intersect-domain %isl-union-map-intersect-domain
  (:give union-map)
  (:take union-map)
  (:take union-set))
