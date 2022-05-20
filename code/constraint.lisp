#|
(in-package :cl-isl)

(define-isl-object constraint
  :abstract t
  :free %isl-constraint-free
  :copy %isl-constraint-copy
  :list-type constraint-list)

(defmethod print-object ((constraint constraint) stream)
  (print-unreadable-object (constraint stream :type t)
    (let ((aff (%isl-constraint-get-aff (constraint-handle constraint))))
      (unwind-protect (write-string (%isl-aff-to-str aff) stream)
        (%isl-aff-free aff)))))

(define-isl-object equality-constraint
  :superclass constraint)
(define-isl-function make-equality-constraint %isl-constraint-alloc-equality
  (:give equality-constraint)
  (:take local-space))
(define-isl-function equality-constraint-set-constant %isl-constraint-set-constant-val
  (:give equality-constraint)
  (:take equality-constraint)
  (:take value))
(define-isl-function equality-constraint-set-coefficient %isl-constraint-set-coefficient-val
  (:give equality-constraint)
  (:take equality-constraint)
  (:keep t dim-tyme-todo)
  (:keep integer position)
  (:take value value))

(define-isl-object inequality-constraint
  :superclass constraint)
(define-isl-function make-inequality-constraint %isl-constraint-alloc-inequality
  (:give inequality-constraint)
  (:take local-space))
(define-isl-function inequality-constraint-set-constant %isl-constraint-set-constant-val
  (:give inequality-constraint)
  (:take inequality-constraint)
  (:take value))
(define-isl-function inequality-constraint-set-coefficient %isl-constraint-set-coefficient-val
  (:give inequality-constraint)
  (:take inequality-constraint)
  (:keep t dim-tyme-todo)
  (:keep integer position)
  (:take value value))
|#


(in-package :cl-isl)

(define-isl-object constraint
  ;;:abstract t
  :free %isl-constraint-free
  :copy %isl-constraint-copy
  :list-type constraint-list)

(defmethod print-object ((constraint constraint) stream)
  (print-unreadable-object (constraint stream :type t)
    (let ((aff (%isl-constraint-get-aff (constraint-handle constraint))))
      (unwind-protect (write-string (%isl-aff-to-str aff) stream)
        (%isl-aff-free aff)))))

(define-isl-object equality-constraint
  :superclass constraint)

(define-isl-object inequality-constraint
  :superclass constraint)

(define-isl-function make-equality-constraint %isl-equality-alloc
  (:give constraint)
  (:take local-space))

(define-isl-function make-inequality-constraint %isl-inequality-alloc
  (:give constraint)
  (:take local-space))

(define-isl-function constraint-s %isl-constraint-set-constant-val
  (:give constraint)
  (:take constraint)
  (:take value))

(define-isl-function constraint-set-constant-si %isl-constraint-set-constant-si
  (:give constraint)
  (:take constraint)
  (:keep integer))

(define-isl-function constraint-set-coefficient-si %isl-constraint-set-coefficient-si
  (:give constraint)
  (:take constraint)
  (:keep t dim-type)
  (:keep integer pos)
  (:keep integer value))

(define-isl-function constraint-alloc-equality %isl-constraint-alloc-equality
  (:give constraint)
  (:take local-space))

(define-isl-function constraint-alloc-inequality %isl-constraint-alloc-inequality
  (:give constraint)
  (:take local-space))
