(in-package :cl-isl)

(define-isl-object point :free %isl-point-free :copy %isl-point-copy)

(defmethod print-object ((value point) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-point-to-str (point-handle value)) stream)))

;; What type of point it is
(define-isl-function point-zero %isl-point-zero
  (:give point)
  (:take space))

(defun point-set-coord-2 (p type a b)
  (let ((r
          (%make-point
           (%isl-point-set-coordinate-val
            (point-handle (copy p))
            type
            a
            (%isl-val-int-from-si (context-handle *context*) b)))))
;;    (break "~a" r)
    r))

;; get/set coordinate: todo with type

;; point -> set

(define-isl-function basic-set-from-point %isl-basic-set-from-point
  (:give basic-set)
  (:take point))

(define-isl-function set-from-point %isl-set-from-point
  (:give set)
  (:take point))

(define-isl-function union-set-from-point %isl-union-set-from-point
  (:give union-set)
  (:take point))

;; (point, point) -> set

(define-isl-function basic-set-box-from-points %isl-basic-set-box-from-points
  (:give basic-set)
  (:take point)
  (:take point))

(define-isl-function set-box-from-points %isl-set-box-from-points
  (:give set)
  (:take point)
  (:take point))

;; Sample points

(define-isl-function basic-set-sample-point %isl-basic-set-sample-point
  (:give basic-set)
  (:take point))

(define-isl-function set-sample-point %isl-set-sample-point
  (:give set)
  (:take point))

(define-isl-function union-set-sample-point %isl-set-sample-point
  (:give union-set)
  (:take point))
