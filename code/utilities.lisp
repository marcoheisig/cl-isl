(in-package #:cl-isl)

(defun make-isl-sym (&rest things)
  (intern
   (apply #'concatenate 'string (mapcar #'string things))
   (find-package "CL-ISL")))

(defun string-from-symbol (symbol)
  (let ((*package* (find-package "KEYWORD"))
        (*print-readably*)
        (*print-case* :upcase))
    (with-output-to-string (stream)
      (format stream "~S" symbol))))
