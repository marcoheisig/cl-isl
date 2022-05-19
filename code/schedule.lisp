(in-package :cl-isl)

;; Schedule

(define-isl-object schedule
  :free %isl-schedule-free
  :copy %isl-schedule-copy)

(defmethod print-object ((value schedule) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-schedule-to-str (schedule-handle value)) stream)))

;; Creation - from schedule constraints

(define-isl-object schedule-constraints
  :free %isl-schedule-constraints-free
  :copy %isl-schedule-constraints-copy)

(define-isl-function schedule-constraints-compute-schedule %isl-schedule-constraints-compute-schedule
  (:give schedule)
  (:take schedule-constraints))

(defmethod print-object ((value schedule-constraints) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-schedule-constraints-to-str (schedule-constraints-handle value)) stream)))

;; Creation

(define-isl-function schedule-constraints-on-domain %isl-schedule-constraints-on-domain
  (:give schedule-constraints)
  (:take union-set))

;; Binary operations

(define-isl-function schedule-constraints-set-validity %isl-schedule-constraints-set-validity
  (:give schedule-constraints)
  (:take schedule-constraints)
  (:take union-map))

(define-isl-function schedule-constraints-set-coincidence %isl-schedule-constraints-set-coincidence
  (:give schedule-constraints)
  (:take schedule-constraints)
  (:take union-map))
(define-isl-function schedule-constraints-set-proximity %isl-schedule-constraints-set-proximity
  (:give schedule-constraints)
  (:take schedule-constraints)
  (:take union-map))
;; Ast build - new file?

(define-isl-object ast-build
  :free %isl-ast-build-free
  :copy %isl-ast-build-copy)
;; Cannot print this object

(define-isl-function ast-build-alloc %isl-ast-build-alloc
  (:give ast-build)
  (:parm context *context*))

(define-isl-function ast-build-node-from-schedule %isl-ast-build-node-from-schedule
   (:give ast-node)
   (:take ast-build)
   (:take schedule))

(define-isl-function ast-build-node-from-schedule-map %isl-ast-build-node-from-schedule-map
  (:give ast-node)
  (:take ast-build)
  (:take union-map))


(when nil
(defun get-custom (domain read-access write-access initial-schedule)
  (let* ((ctx (isl-object-handle *context*))

         (before-map (union-map initial-schedule initial-schedule))

         (read-access (intersect read-access domain))
         (write-access (intersect write-access domain))

         (RaW (intersect-map (before write-access (inverse read-access)) before-map))
         (WaW (intersect-map (before write-access (inverse write-access)) before-map))
         (WaR (intersect-map (before read-access (inverse write-access)) before-map))

         (total (union-map (union-map RaW WaW) WaR))

         (schedule (%isl-schedule-constraints-on-domain domain))
         (schedule (%isl-schedule-constraints-set-validity schedule total))
         (schedule (%isl-schedule-constraints-set-coincidence schedule RaW))
         (schedule (%isl-schedule-constraints-compute-schedule schedule))

         (ast-build (%isl-ast-build-alloc ctx))
         (node (%isl-ast-build-node-from-schedule ast-build schedule)))
    node))
)

(defun get-my-custom (domain read-access write-access initial-schedule)
  (let* ((before-map (union-map-lex-lt-union-map initial-schedule initial-schedule))

         (read-access (union-map-intersect-domain read-access domain))
         (write-access (union-map-intersect-domain write-access domain))

         (RaW (union-map-intersect
               (union-map-apply-range write-access (union-map-reverse read-access))
               before-map))
         (WaW (union-map-intersect
               (union-map-apply-range write-access (union-map-reverse write-access))
               before-map))
         (WaR (union-map-intersect
                (union-map-apply-range read-access (union-map-reverse write-access))
                before-map))
         (RaR (union-map-intersect
               (union-map-apply-range read-access (union-map-reverse read-access))
               before-map))
         #+or(RaR (union-map-intersect-domain
               (union-map-from-str " { [i0, i1, i2] -> [i0', i1, i2'] : i2 <= i2' } ")
               domain))

         (total (union-map-union (union-map-union RaW WaW) WaR))

         (schedule (schedule-constraints-on-domain domain))
         (schedule (schedule-constraints-set-validity schedule total))
         (schedule (schedule-constraints-set-coincidence schedule RaW))
         (ok (isl::union-map-from-str " { [i0, i1, i2] -> [i0', i1, i2] : i0' > i0; [2, i1+1, i2] -> [0, i1, i2]} "))
         (schedule (schedule-constraints-set-proximity schedule ok))
         (schedule (schedule-constraints-compute-schedule schedule))

         (ast-build (ast-build-alloc))
         (node (ast-build-node-from-schedule ast-build schedule)))
    node))


(defun get-my-initial-result (domain read-access write-access initial-schedule)
  (declare (ignore domain read-access write-access))
  (let* ((ast-build (ast-build-alloc))
         (ast-node (ast-build-node-from-schedule-map ast-build initial-schedule)))
    ast-node))

(defun get-initial-result (domain read-access write-access initial-schedule)
  (declare (ignore domain read-access write-access))
  (let* ((ctx (isl-object-handle *context*))

         ;;(before-map (union-map initial-schedule initial-schedule))

         ;;(read-access (intersect read-access domain))
         ;;(write-access (intersect write-access domain))

         ;;(RaW (intersect-map (before write-access (inverse read-access)) before-map))
         ;;(WaW (intersect-map (before write-access (inverse write-access)) before-map))
         ;;(WaR (intersect-map (before read-access (inverse write-access)) before-map))

         ;;(total (union-map (union-map RaW WaW) WaR))

         ;;(schedule (%isl-schedule-constraints-on-domain domain))
         ;;(schedule (%isl-schedule-constraints-set-validity schedule total))
         ;;(schedule (%isl-schedule-constraints-set-coincidence schedule RaW))
         ;;(schedule (%isl-schedule-constraints-compute-schedule schedule))
         (ast-build (%isl-ast-build-alloc ctx))
         (ast-node (%isl-ast-build-node-from-schedule-map ast-build initial-schedule)))
    ast-node))

(defun print-node (node)
  (print (%isl-ast-node-to-C-str (isl-object-handle node))))

