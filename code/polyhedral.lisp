;; Everything related to the polyhedral part of ISL
;; Schedule, code generation, etc...

(in-package #:cl-isl)

(create-object ast_node :printable nil)
(create-object ast_build :printable nil :alloc t)
(create-object schedule :printable nil)


;; Useless
(defun ast_build-from-context (e)
  (check-type e isl-set)
  (create-ast_build (isl_ast_build_from_context (obj e))))

(defun ast_build-from-node-from-schedule-map (a m)
  (check-type a isl-ast_build)
  (check-type m isl-union_map)
  (create-ast_node (isl_ast_build_node_from_schedule_map (obj a) (obj m))))

;; Useful
(defun create-schedule-on-domain (set)
  (check-type set isl-union_set)
  (create-schedule (isl_schedule_constraints_on_domain (obj set))))

(defun schedule-constraints-set-validity (schedule map)
  (check-type schedule isl-schedule)
  (check-type map isl-union_map)
  (create-schedule
   (isl_schedule_constraints_set_validity
    (obj schedule)
    (obj map))))

(defun schedule-constraints-compute-schedule (schedule)
  (check-type schedule isl-schedule)
  (create-schedule
   (isl_schedule_constraints_compute_schedule (obj schedule))))

(defun ast_build-node-from-schedule (ast schedule_)
  (let ((schedule (copy-object schedule_)))
    (check-type ast isl-ast_build)
    (check-type schedule isl-schedule)
    (create-ast_build (isl_ast_build_node_from_schedule (obj ast) (obj schedule)))))

(defmacro defun-with-type (name args &rest code)
  `(defun ,name ,(mapcar #'first args)
     ,(append
       '(progn)
       ;; Check of types
       (mapcar (lambda (var type) `(check-type ,var ,type)) (mapcar #'first args) (mapcar #'second args))
       ;; Copy of variable
       `((let
             ,(remove-if ; Remove when we don't copy the variable
               #'not
               (mapcar (lambda (var take) (when (eql take 'take) `(,var (copy-object ,var))))
                       (mapcar #'first args) (mapcar #'third args)))
           ;; The actual code of the function
           ,(cons 'progn code))))))

(defun-with-type ast_build-node-from-schedule ((ast isl-ast_build keep) (schedule isl-schedule take))
  (create-ast_build (isl_ast_build_node_from_schedule (obj ast) (obj schedule))))

(defun ast_node-to-C-str (a)
  (check-type a isl-ast_build)
  (isl_ast_node_to_C_str (obj a)))


