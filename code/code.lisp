(in-package #:cl-isl)
(ql:quickload :trivial-garbage) ; Used to collect the C memory when the lisp object is collected

(defparameter *context* (isl_ctx_alloc)) ; Whatever, for threads or idk
(defparameter *print* (isl_printer_to_str *context*)) ; Maybe useful some day to print things
(defparameter *space* (isl_space_unit *context*))

;; Execute the function, and catch the error
(defmacro wrap-for-error (function)
  `(progn
     (let ((answer ,function))
       (let ((e (isl_ctx_last_error_msg *context*)))
         (isl_ctx_reset_error *context*)
         (when e (break "<<~a>> when executing ~a" e ',function)))
       answer)))
(defun print-error () (print isl_ctx_last_erorr_msg *context*))
(defmacro defun-with-error (name args &rest code) `(defun ,name ,args ,(list 'wrap-for-error (cons 'progn code))))
(defmacro with-error (code) (subst 'defun-with-error 'defun (macroexpand code)))

;; Currify to give *context* as the first argument on all isl_... calls
;;(defmacro with-context (code) (subst 'defun-with-context 'defun (macroexpand code)))

;; Maybe we want to copy objects
(defgeneric copy-object (e))

;; Create a custom objects and all the methods for each isl type
(defmacro create-print-object (type &key
                                      (printable t) ; If the object can be read from strings and be printed
                                      (free nil) ; Should we free memory. Should be t by default
                                      (alloc nil) ; If the object is created with _alloc, or with _empty
                                      ;; space/context/... are _alloc, otherwise it's _empty. Ast_build is _alloc
                                      )
  ;; FIRST, SOME HELPER FUNCTIONS
  (flet (
         ;; Concatenation of strings to form a symbol
         (++ (&rest rest) (read-from-string (apply #'concatenate (cons 'string rest))))
         )
    (let* (
           (s-type (format nil "~a" type)) ; the type in string. Right now basic_set and not basic-set
           (type (++ "isl-" s-type))
           ;; Wrappers around the C object
           (create-object (++ "create-" s-type))
           ;; Creation of empty objects - returns a lisp object which is a wrapper around the empty C object
           (alloc-object (when alloc (++ "alloc-" s-type)))
           (create-empty-object (unless alloc (++ "create-empty-" s-type)))
           ;; Is the underlying object empty
           (empty-object-p (++ "empty-" s-type "-p"))
           ;; Free the underlying object. Todo, not exported for the user
           (free-object-library (++ "isl-" s-type "-free"))
           )
      ;; THE CODE GENERATED STARTS HERE
      (with-error
          `(progn
             ;; CREATE THE CLASS
             (defclass ,type () ((obj :initarg :obj :accessor obj)))
             ;; Free -- This shouldn't be used by the user, only by trivial-garbage
             ,(let ((name-library (++ "isl_" s-type "_free")))
                (if free
                    `(defun ,free-object-library (e) (,name-library e))
                    `(defun ,free-object-library (e) ())))
             ;; Create the wrapper around the C object
             (defun ,create-object (e)
               (when (eql ',type (type-of e)) "Your object is already a lisp object. Maybe you wanted to copy your object?")
               (let ((answer (make-instance ',type :obj e)))
                 (trivial-garbage:finalize answer (lambda () (,free-object-library e)))
                 answer))
             ;; Create/alloc empty object
             ,(if alloc
                  ;; When the object is created with _allow
                  (let ((name-library (++ "isl_" s-type "_alloc")))
                    `(defun ,alloc-object ()
                       (,create-object (,name-library *context*)))) ; unclear what to do to free memory
                  ;; When the object is created with _empty
                  (let ((name-library (++ "isl_" s-type "_empty")))
                    `(defun ,create-empty-object () ; Yes below it's *space* and not *context*
                       (,create-object (,name-library *space*))))) ; unclear what to do to free memory
             ;; Check if the object is empty
             ,(let ((name-library (++ "isl_" s-type "_is_empty")))
                `(defun ,empty-object-p (object)
                   (create-lisp-bool (,name-library (obj object)))))
             ;; Copy -- Not tested yet
             ,(let ((name-library (++ "isl_" s-type "_copy")))
                `(defmethod copy-object ((e ,type)) (,create-object (,name-library (obj e)))))
             ;; FROM/TO STRING - when printable obly
             ,(when printable
                (let ((name-library (++ "isl_" s-type "_to_str")))
                  `(defmethod print-object ((object ,type) out)
                     (format out (,name-library (obj object))))))
             ,(let ((name-library (++ "isl_" s-type "_read_from_str"))
                    (name-me (++ s-type "-read-from-str")))
                `(progn
                   (defun ,name-me (str)
                     (check-type str string)
                     (,create-object (,name-library *context* str)))))
             )))))

;; Convert an isl-bool to a lisp boolean
(defun create-lisp-bool (obj)
  (cond
    ((eql obj :isl-bool-true) t)
    ((eql obj :isl-bool-false) nil)
    ((eql obj :isl-bool-error) (break "You try to convert a bool that is an error"))
    (t (break "You try to convert ~a, and it's not an isl-bool" obj))))

(create-print-object bool :free nil :printable nil) ; unclear if we should create objects for bools and ints. Right now lisp bool are used
(create-print-object basic_set)
(create-print-object union_set)
(create-print-object union_map)
(create-print-object ast_node :printable nil)
(create-print-object ast_build :printable nil :alloc t)
(create-print-object set)
(create-print-object schedule)
(create-print-object domain)



(defun isl_bool_to_str (obj) (create-lisp-bool obj))
(defmethod print-object ((object isl-bool) out) (format t "~a" (isl_bool_to_str (obj object))))

(defun ast_build-from-context (e)
  (check-type e isl-set)
  (create-ast_build (isl_ast_build_from_context (obj e))))

(defun ast_build-from-node-from-schedule-map (a m)
  (check-type a isl-ast_build)
  (check-type m isl-union_map)
  (create-ast_node (isl_ast_build_node_from_schedule_map (obj a) (obj m))))

(defun ast_node-to-C-str (a)
  (check-type a isl-ast_node)
  (isl_ast_node_to_C_str (obj a)))
