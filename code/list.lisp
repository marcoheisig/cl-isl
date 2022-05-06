;; The isl library define a "list" type for multiple types (set, map, ...)
;; It is used to take/return a variable number of arguments
;; For instance, for an ast, a block is a group of instruction -> a list of ast-expr

;; A naive idea would be to have a base type "isl-list", and inherit from it for each type
;; However, 2 problems:
;; + It seeems we can't know the type of a list once we have it
;;       For instance ast-expr, we can know if it's a ast-expr-op, ast-expr-int, ...
;; + The base class can't exist by itself, it doesn't have a free, a copy, etc...
;;       And all operations are named differently for each classes
;; Hence the decision to build a list type for each type concerned with no relation between each others!
;; Right now, only ast-block, and todo loop over types to generate all possible versions
(in-package :cl-isl)

(define-isl-entity ast-node-list
  :free %isl-ast-node-list-free
  :copy %isl-ast-node-list-copy)

(defmethod isl-entity-plist ((value ast-node-list))
  (list :str (%isl-ast-node-list-to-str (isl-entity-handle value))))

(defmethod print-object ((value ast-node-list) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-ast-node-list-to-str (ast-node-list-handle value)) stream)))

(define-isl-function isl-ast-node-list-size %isl-ast-node-list-size
  (:give (signed-byte 64))
  (:keep ast-node-list))

(define-isl-function isl-ast-node-list-get-at %isl-ast-node-list-get-at
  (:give ast-node)
  (:keep ast-node-list)
  (:keep (signed-byte 64)))

(defun ast-node-list-foreach (ast-node-list function)
  (let ((n (isl-ast-node-list-size ast-node-list)))
    (loop for i below n do
      (funcall function (isl-ast-node-list-get-at ast-node-list i)))))
