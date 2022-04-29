(in-package #:cl-isl)

(defun make-isl-sym (&rest things)
  (intern
   (apply #'concatenate 'string (mapcar #'string things))
   (find-package "CL-ISL")))

(defun lispify-isl-bool (isl-bool)
  (ecase isl-bool
    (:isl-bool-true t)
    (:isl-bool-false nil)
    (:isl-bool-error
     (isl-bool-error))))

(defun isl-bool-error ()
  (break "TODO"))
