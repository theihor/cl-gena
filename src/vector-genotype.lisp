(defpackage :cl-gena/vector-genotype
  (:use :common-lisp :cl-gena/generic-defs :cl-gena/random))

(in-package :cl-gena/vector-genotype)

(defclass vector-genotype (genotype)
  ((sequence :accessor get-sequence
             :initarg :sequence
             :initform #())
   (size :reader size
         :initarg :size) 
   (lower-bound :reader lower-bound
                :initarg :lower-bound)
   (upper-bound :reader upper-bound
                :initarg :upper-bound)))

(defclass gv-population (population) ())

(defmethod print-object ((g vector-genotype) s)
  (format s "#<VG [ ")
  (loop for x across (get-sequence g)
     do (format s "~,4F~T" x))
  (format s "]>"))

(defmethod print-object ((p gv-population) s)
  (format s "#<VG-POPULATION ~A [" (size p))
  (loop for g in (genotype-list p)
     do (format s "~%    ~A" g))
  (format s " ]>"))

(defun vector-genotype-generator (size lower-bound upper-bound)
  (lambda (index)
    (declare (ignore index))
    (make-instance 'vector-genotype
                   :sequence (loop with array = (make-array size)
                                for i from 0 below size
                                do (setf (aref array i)
                                         (random-float :from lower-bound
                                                       :to upper-bound))
                                finally (return array))
                   :size size
                   :lower-bound lower-bound
                   :upper-bound upper-bound)))

(defmethod select ((p gv-population))
  )
