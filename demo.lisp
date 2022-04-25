(ql:quickload :cl-isl)

(in-package #:cl-isl)

;; Test
(let* ((a (basic_set-read-from-str "{[i] : exists (a : i = 2a and i >= 10 and i <= 42)}"))
       (emp (create-empty-basic_set)))
  (print (empty-basic_set-p emp))
  (print (empty-basic_set-p a))
  (print (obj a))
  (assert (empty-basic_set-p emp))
  (assert (not (empty-basic_set-p a)))
  )


(defmacro break2 (a) ())

(let ((t1 "{TAB[I] -> [I,1] : 0 <= I < 100;T2[I] -> [I,2] : 0 <= I < 100;}")
      (t2 "{TAB[I,J] -> [I,2,J,3] : 0 <= I < 100 and 0 <= J < 100;}")
      (t3 "{TAB[I,J] -> [I,3,J,4] : 0 <= I < 100 and 0 <= J < 100;}")
      (t4 "{C[I,J] -> [I,4,J,5] : 0 <= I < 100 and 0 <= J < 100;C1[I,J] -> [I,4,J,5,K,6] : 0 <= I < 100 and 0 <= J < 100 and 0 <= K < 100;C2[I,J] -> [I,4,J,5,K,7] : 0 <= I < 100 and 0 <= J < 100 and 0 <= K < 100;A[I,K] -> [I,4,J,5,K,8] : 0 <= I < 100 and 0 <= J < 100 and 0 <= K < 100;B[K,J] -> [I,4,J,5,K,9] : 0 <= I < 100 and 0 <= J < 100 and 0 <= K < 100;}")
      (t5 "[N] -> { S0[i] -> [i, 0] : 0 <= i < N; S1[i] -> [i, 1] : 0 <= i < N }")
      )

  ;; Previous test
  (let ((s t4))
    (print s)
    (let* ((map (union_map-read-from-str s))
           (ast-build (ast_build-from-context (set-read-from-str "[N] -> { : }")))
           (ast-node (ast_build-from-node-from-schedule-map ast-build map)))
      (print (isl_ast_node_to_C_str (obj ast-node)))))

  ;; Demo with and without the library
  ;; First, without
  (let* ((ast-build (isl_ast_build_alloc *context*)) ;;from_context (isl_set_read_from_str *context* "[N] -> { : }")))
                                        ;(schedule (isl_union_map_read_from_str *context* t1))
         (schedule (isl_schedule_constraints_on_domain
                    (isl_union_set_read_from_str *context* "[N] -> { A[i]: 0 <= i < N }")))
         (_ (break2 "Domain done"))
         (schedule (isl_schedule_constraints_set_validity
                    schedule
                    (isl_union_map_read_from_str *context* "[N] -> { A[i] -> A[i-1]: 0 <= i}")))
         (_ (break2 "Validity done"))
         (_ (break2 "End of creation of the schedule"))
         (schedule (isl_schedule_constraints_compute_schedule schedule))
         (_ (break2 "Schedule created"))
         (node (isl_ast_build_node_from_schedule ast-build schedule))
         (_ (break2 "Ast created"))
         )
    (print (isl_ast_node_to_C_str node)))

  ;; And then, with cl-isl
  (let* ((ast-build (alloc-ast_build))
         (schedule (create-schedule-on-domain
                    (union_set-read-from-str "[N] -> { A[i, j]: 0 <= i < N and 0 <= j < N; B[i, j]: 0 <= i < N and 0 <= j < N; RES[i, j]: 0 <= i < N and 0 <= j < N}")))
         (_ (break2 "Domain done"))
         (schedule1 (schedule-constraints-set-validity
                     schedule
                     (union_map-read-from-str "[N] -> { B[i, j] -> A[i, j] : 0 <= i}")))
         (_ (break2 "Validity done"))
         (_ (break2 "End of creation of the schedule"))
         (schedule2 (schedule-constraints-compute-schedule schedule1))
         (_ (break2 "Schedule created"))
         (node (ast_build-node-from-schedule ast-build schedule2))
         (_ (break2 "Ast created"))
         )
    (print (ast_node-to-C-str node)))

  )


















