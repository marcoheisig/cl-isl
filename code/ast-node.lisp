(in-package :cl-isl)

(define-isl-object ast-node
  :abstract t
  :free %isl-ast-node-free
  :copy %isl-ast-node-copy
  :list-type ast-node-list)

(defmethod print-object ((ast ast-node) stream)
  (print-unreadable-object (ast stream :type t)
    (write-string (%isl-ast-node-to-str (ast-node-handle ast)) stream)))

(define-isl-function node-get-type %isl-ast-node-get-type
  (:give ast-expr-type)
  (:keep ast-node))

;; FOR NODE

(define-isl-object for-node
  :superclass ast-node)

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give ast-expr)
                (:keep for-node))))
  (def for-node-get-iterator %isl-ast-node-for-get-iterator)
  (def for-node-get-init %isl-ast-node-for-get-init)
  (def for-node-get-cond %isl-ast-node-for-get-cond)
  (def for-node-get-inc %isl-ast-node-for-get-inc))

(define-isl-function for-node-get-body %isl-ast-node-for-get-body
  (:give ast-node)
  (:keep for-node))

;; IF NODE

(define-isl-object if-node
  :superclass ast-node)

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give ast-expr)
                (:keep if-node))))
  (def if-node-get-cond %isl-ast-node-if-get-cond)
  (def if-node-get-then %isl-ast-node-if-get-then)
  (def if-node-get-else %isl-ast-node-if-get-else))

(define-isl-function if-node-hash-else %isl-ast-node-if-has-else
  (:give boolean)
  (:keep ast-node))

;; USER NODE

(define-isl-object user-node
  :superclass ast-node)

(define-isl-function user-node-get-expr %isl-ast-node-user-get-expr
  (:give ast-expr)
  (:keep user-node))


;; BLOCK NODE - a sequence of instruction

(define-isl-object block-node
  :superclass ast-node)

(define-isl-function block-node-getlist %isl-ast-node-block-get-children
  (:give ast-node-list)
  (:keep block-node))

;; MARK NODE

(define-isl-object mark-node
  :superclass ast-node)

;; The rest - todo

(defun %make-ast-node (handle)
  (ecase (%isl-ast-node-get-type handle)
    (:ast-expr-error (isl-error))
    (:ast-node-for (%make-for-node handle))
    (:ast-node-if (%make-if-node handle))
    (:ast-node-block (%make-block-node handle))
    (:ast-node-mark (%make-mark-node handle))
    (:ast-node-user (%make-user-node handle))))

;; Temporary creating the ast

;; Generics functions
(defun inverse (e) (%isl-union-map-reverse
                    (%isl-union-map-copy e)))

(defun intersect (m d)
  (%isl-union-map-intersect-domain
   (%isl-union-map-copy m)
   (%isl-union-set-copy d)))

(defun intersect-map (e m)
  (%isl-union-map-intersect
   (%isl-union-map-copy e)
   (%isl-union-map-copy m)))

(defun union-map (a b) (%isl-union-map-union
                        (%isl-union-map-copy a)
                        (%isl-union-map-copy b)))

;(defun-with-type jsp ((a isl-union-map take) (b isl-union-map take))
;  (create-union-map (isl-union-map-lex-lt-union-map (obj a) (obj b))))

(defun before (a b) (%isl-union-map-apply-range
                     (%isl-union-map-copy a)
                     (%isl-union-map-copy b)))

(defun get-perfect-ast (s-domain s-read s-write s-schedule)
  (let* ((ctx (isl-object-handle *context*))
         (domain (%isl-union-set-read-from-str ctx s-domain))
         (read-access (%isl-union-map-read-from-str ctx s-read))
         (write-access (%isl-union-map-read-from-str ctx s-write))
         (initial-schedule (%isl-union-map-read-from-str ctx s-schedule))

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


(defun get-initial-result (domain read-access write-access initial-schedule)
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
