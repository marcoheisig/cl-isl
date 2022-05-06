(in-package :cl-isl)

(define-isl-entity constraint
  :free nil ;; We'll write only function that takes constraint, hopefully no memory will leak
  :copy (lambda (c) (break "You tried to copy a constraint, you can't!!!!")))

(defmethod isl-entity-plist ((value constraint))
  (list :str (%isl-constraint-to-str (isl-entity-handle value))))

;; It's not possible to print a constraint

(defun constraint-set-constant-si (c i)
  (%make-constraint
   (%isl-constraint-set-constant-si (isl-entity-handle c) i)))

(defun constraint-set-coefficient-si (c type a b)
  (%make-constraint
   (%isl-constraint-set-coefficient-si (isl-entity-handle c) type a b)))

(define-isl-function constraint-alloc-equality %isl-constraint-alloc-equality
  (:give constraint)
  (:take local-space))

(define-isl-function constraint-alloc-inequality %isl-constraint-alloc-inequality
  (:give constraint)
  (:take local-space))
