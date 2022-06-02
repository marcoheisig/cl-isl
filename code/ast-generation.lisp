(in-package :cl-isl)

(define-isl-function ast-build-node-from-schedule %isl-ast-build-node-from-schedule
   (:give ast-node)
   (:take ast-build)
   (:take schedule))

(define-isl-function ast-build-node-from-schedule-map %isl-ast-build-node-from-schedule-map
  (:give ast-node)
  (:take ast-build)
  (:take union-map))

(defun generate-debug-ast (domain read-access write-access initial-schedule)
  (declare (ignore domain read-access write-access))
  (let* ((ast-build (create-ast-build))
         (ast-node (ast-build-node-from-schedule-map ast-build initial-schedule)))
    ast-node))

(defun generate-optimized-ast (domain read-access write-access initial-schedule)
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
         #+(or)
         (RaR (union-map-intersect
               (union-map-apply-range read-access (union-map-reverse read-access))
               before-map))
         (total (union-map-union (union-map-union RaW WaW) WaR))

         (sconstraint (schedule-constraints-on-domain domain))
         (sconstraint (schedule-constraints-set-validity sconstraint total))
         (sconstraint (schedule-constraints-set-coincidence sconstraint RaW))

         ;; Proximity
         ;; Read: domain -> read access
         ;; Read^1 : read access -> domain
         ;; What we want: map (Read^1 (memory location)) to (Read^1 (next memory location))
         (memory->domain (union-map-reverse (union-map-union read-access write-access)))
         (memory-proximity (union-map-from-str
                            "{ [i0, i1, i2, i3, i4] -> [i0, i1+1, i2, i3, i4] } "))
         (memory-proximity (union-map-apply-range memory-proximity memory->domain))
         (memory-proximity (union-map-apply-domain memory-proximity memory->domain))
         ;;(_ (print memory-proximity))
         (sconstraint (schedule-constraints-set-proximity sconstraint memory-proximity))
         (schedule (schedule-constraints-compute-schedule sconstraint))

         (ast-build (create-ast-build))
         (node (ast-build-node-from-schedule ast-build schedule)))
    (print node)
    node))
