(in-package #:cl-isl)

(deftype size ()
  '(and unsigned-byte fixnum))

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

(defun lispify-isl-size (isl-size)
  (if (= isl-size +isl-size-error+)
      (isl-size-error)
      (the size isl-size)))

(defun isl-bool-error ()
  (break "TODO"))

(defun isl-size-error ()
  (break "TODO"))
