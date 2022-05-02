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
     (isl-error))))

(defun lispify-isl-size (isl-size)
  (if (= isl-size +isl-size-error+)
      (isl-error)
      (the size isl-size)))

(defun string-from-symbol (symbol)
  (let ((*package* (find-package "KEYWORD"))
        (*print-readably*)
        (*print-case* :upcase))
    (with-output-to-string (stream)
      (format stream "~S" symbol))))

(defun isl-error ()
  (break "TODO"))
