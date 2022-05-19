(in-package :cl-isl)

(define-isl-function ast-build-node-from-schedule %isl-ast-build-node-from-schedule
   (:give ast-node)
   (:take ast-build)
   (:take schedule))

(define-isl-function ast-build-node-from-schedule-map %isl-ast-build-node-from-schedule-map
  (:give ast-node)
  (:take ast-build)
  (:take union-map))

(defun get-new-result (domain read-access write-access initial-schedule)
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
         (ok (union-map-from-str " { [i0, i1, i2] -> [i0', i1, i2] : i0' > i0; [2, i1, i2] -> [0, i1, i2+1]} "))
         (schedule (schedule-constraints-set-proximity schedule ok))
         (schedule (schedule-constraints-compute-schedule schedule))

         (ast-build (create-ast-build))
         (node (ast-build-node-from-schedule ast-build schedule)))
    node))


(defun get-initial-result (domain read-access write-access initial-schedule)
  (declare (ignore domain read-access write-access))
  (let* ((ast-build (create-ast-build))
         (ast-node (ast-build-node-from-schedule-map ast-build initial-schedule)))
    ast-node))
