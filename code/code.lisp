(in-package #:cl-isl)

(defparameter *print* (%isl-printer-to-str (handle-of *context*))) ; Maybe useful some day to print things
(defmacro new-space () '(%isl-space-unit (handle-of *context*))) ; Is consumed everytime it's used. (Not yet but) its value can be changed during the execution


;; Execute the function, and catch the error
(defmacro wrap-for-error (function)
  `(progn
     (let ((answer ,function))
       (let ((e (%isl-ctx-last-error-msg (handle-of *context*))))
         (%isl-ctx-reset-error (handle-of *context*))
         (when e (break "<<~a>> when executing ~a" e ',function)))
       answer)))
(defun print-error () (print (%isl-ctx-last-erorr-msg (handle-of *context*))))
(defmacro defun-with-error (name args &rest code) `(defun ,name ,args ,(list 'wrap-for-error (cons 'progn code))))
(defmacro with-error (code) (subst 'defun-with-error 'defun (macroexpand code)))


;; Create a function with type
;; Usage: (defun-with-type f ((ast isl-ast-build keep) (schedule isl-schedule take)) 'thecode)
;; Ensure arguments have the good type with 'check-type, and copy them when they are marked 'take
(defmacro defun-with-type (name args &rest code)
  ;; Verify the user gave either take or give for the 3rd part
  (unless (every (lambda (a) (member a '(take keep))) (mapcar #'third args))
    (break "Your arguments are ~a, and it needs to be either take/keep for the 3rd part" args))
  ;; The actual code
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


;; Makes sure the C function is defined when created by the macro
;; Todo

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun my-fboundp (f)
    (when (fboundp f) f)))

;; Crawl a source code and break on each nil function call
(defun safe-code (code &optional (first-call t))
  (unless (atom code)
    (unless (first code)
      (break "~a is a unsafe generation" code)) ;; The error gives the trace of the code generation to debug
    (mapcar (lambda (a) (safe-code a nil)) code))
  (when first-call (eval code)))

(safe-code
`(defun ff (a) (+ a 109)))

(safe-code
 `(progn
    ;(defun f (a) (+ a (nil a)))
    (print 10)))


;; Maybe we want to copy objects
(defgeneric copy-object (e))

;; Create a custom objects and all the methods for each isl type
(defmacro create-object (type &key
                                (printable t) ; If the object can be read from strings and be printed
                                (free nil) ; Should we free memory. Should be t by default
                                (alloc nil) ; If the object is created with _alloc, or with _empty
                                ;; space/context/... are _alloc, otherwise it's _empty. Ast_build is _alloc
                                (conversions nil) ; FROM what the object can be converted to
                                ;; Because it's simpler to write here in the macro
                                )
  ;; FIRST, SOME HELPER FUNCTIONS
  (flet (
         ;; Concatenation of strings to form a symbol
         (++ (&rest rest)
           (let ((result (read-from-string (apply #'concatenate (cons 'string rest)))))
             (if (string= "%isl-" (first rest))
                 (my-fboundp result) ; Will return nil if the C function doesn't exist, which will break when '(defun nil ...)
                 result)))
         )
    (let* (
           (s-type (format nil "~a" type)) ; the type in string. Right now basic_set and not basic-set
           (type (++ "isl-" s-type))
           ;; Wrappers around the C object
           (create-object (++ "create-" s-type))
           ;; Creation of empty objects - returns a lisp object which is a wrapper around the empty C object
           (alloc-object (when alloc (++ "alloc-" s-type)))
           (create-empty-object (unless alloc (++ "create-empty-" s-type)))
           (create-universe-object (unless alloc (++ "create-universe-" s-type)))
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
             ,(let ((name-library (++ "isl-" s-type "-free")))
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
                  (let ((name-library (++ "%isl-" s-type "-alloc")))
                    `(defun ,alloc-object ()
                       (,create-object (,name-library (handle-of *context*)))))
                  ;; When the object is created with _empty, or with _universe
                  (let ((name-library-empty (++ "%isl-" s-type "-empty"))
                        (name-library-universe (++ "%isl-" s-type "-universe")))
                    `(progn
                       (defun ,create-empty-object ()
                         (,create-object (,name-library-empty (new-space))))
                       (defun ,create-universe-object ()
                         (,create-object (,name-library-universe (new-space)))))))
             ;; Check if the object is empty
             ,(let ((name-library (++ "%isl-" s-type "-is-empty")))
                `(defun ,empty-object-p (object)
                   (create-lisp-bool (,name-library (obj object)))))
             ;; Conversion to other types
             ,(cons 'progn
                    (loop for next-type in conversions
                          collect
                          (let* ((s-type2 (format nil "~a" next-type))
                                 (type-2 (++ "isl-" s-type2))
                                 (name-me (++ s-type2 "-to-" s-type))
                                 ;; Sometimes it's either "to", or "from", or both in the doc. So we select whichever is defined
                                 (name1 (++ "%isl-" s-type2 "-to-" s-type))
                                 (name2 (++ "%isl-" s-type "-from-" s-type2))
                                 (name-library (or (my-fboundp name1) (my-fboundp name2)))) ; my-fboundp is nil when the function isn't defined, otherwise just returns
                            `(defun-with-type ,name-me ((e ,type-2 take))
                               (,create-object (,name-library (obj e)))))))

             ;; Copy object
             ,(let ((name-library (++ "%isl-" s-type "-copy")))
                `(defmethod copy-object ((e ,type)) (,create-object (,name-library (obj e)))))
             ;; FROM/TO STRING - when printable obly
             ,(when printable
                (let ((name-library (++ "%isl-" s-type "-to-str")))
                  `(defmethod print-object ((object ,type) out)
                     (format out (,name-library (obj object))))))
             ,(let ((name-library (++ "%isl-" s-type "-read-from-str"))
                    (name-me (++ s-type "-read-from-str")))
                `(progn
                   (defun ,name-me (str)
                     (check-type str string)
                     (,create-object (,name-library (handle-of *context*) str)))))
             )))))

;; Convert an isl-bool to a lisp boolean
(defun create-lisp-bool (obj)
  (cond
    ((eql obj :bool-true) t)
    ((eql obj :bool-false) nil)
    ((eql obj :bool-error) (break "You try to convert a bool that is an error"))
    (t (break "You try to convert ~a, and it's not an isl-bool" obj))))

(create-object bool :free nil :printable nil) ; unclear if we should create objects for bools and ints. Right now lisp bool are used

(defun %isl-bool-to-str (obj) (create-lisp-bool obj))
(defmethod print-object ((object isl-bool) out) (format t "~a" (%isl-bool-to-str (obj object))))

;; Probably want to move boolean/values to a different files

(create-object basic-set)
(create-object union-set)
(create-object union-map)
(create-object set :conversions (basic-set))

(assert (type-of (create-universe-basic-set)) 'isl-basic-set)
(assert (type-of (basic-set-to-set (create-universe-basic-set))) 'isl-set)
