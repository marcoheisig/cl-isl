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


(defvar s-domain)
(setf s-domain "[n] -> {
      S[k] : k <= -2 + 2n and k >= 0;
      T[i, j] : i >= 0 and i <= -1 + n and j <= -1 + n and j >= 0;
}")

(defvar s-read)
(setf s-read "[n] -> {
    T[i, j] -> C[i + j];
    T[i, j] -> B[j];
    T[i, j] -> A[i];
}")

(defvar s-write)
(setf s-write "[n] -> {
    S[k] -> C[k];
    T[i, j] -> C[i + j];
}")

(defvar s-schedule)
(setf s-schedule "[n] -> {
      T[i, j] -> [1, i, j];
      S[k] -> [0, k, 0];
}")

;; Generics functions
(defun-with-type inverse ((e isl-union_map take))
  (create-union_map (isl_union_map_reverse (obj e))))

(defun-with-type intersect ((e isl-union_map take) (d isl-union_set take))
  (create-union_map (isl_union_map_intersect_domain (obj e) (obj d))))

(defun-with-type intersect-map ((e isl-union_map take) (m isl-union_map take))
  (create-union_map (isl_union_map_intersect (obj e) (obj m))))

(defun-with-type union-map ((a isl-union_map take) (b isl-union_map take))
  (create-union_map (isl_union_map_union (obj a) (obj b))))

(defun-with-type jsp ((a isl-union_map take) (b isl-union_map take))
  (create-union_map (isl_union_map_lex_lt_union_map (obj a) (obj b))))

(defun-with-type before ((a isl-union_map take) (b isl-union_map take))
  (create-union_map (isl_union_map_apply_range (obj a) (obj b))))

;; The error printed when we call intersect with the wrongs types is really bad


(let* ((domain (union_set-read-from-str s-domain))
       (read-access (union_map-read-from-str s-read))
       (write-access (union_map-read-from-str s-write))
       (initial-schedule (union_map-read-from-str s-schedule))
       (before (jsp initial-schedule initial-schedule))

       (read-access (intersect read-access domain))
       (write-access (intersect write-access domain))

       (RaW (intersect-map (before write-access (inverse read-access)) before))
       (WaW (intersect-map (before write-access (inverse write-access)) before))
       (WaR (intersect-map (before read-access (inverse write-access)) before))

       (total (union-map (union-map RaW WaW) WaR))


       (ast-build (alloc-ast_build))
       (schedule (create-schedule-on-domain domain))
       (_ (break2 "Domain done"))
       (schedule (schedule-constraints-set-validity schedule total))
       (_ (break2 "Validity done"))
       (_ (break2 "End of creation of the schedule"))
       (schedule (schedule-constraints-compute-schedule schedule))
       (_ (break2 "Schedule created"))
       (node (ast_build-node-from-schedule ast-build schedule))
       (_ (break2 "Ast created"))
       )

  (print (ast_node-to-C-str node))

;;(print read-access)
;;(print (inverse read-access))
;;(print RaW)

)









;; END

(defmacro break2 (a) ())
(when nil
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

  ))
