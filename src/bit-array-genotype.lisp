(defpackage :cl-gena/bit-array-genotype
  (:use :common-lisp :cl-gena/fancy-tools :cl-gena/generic-defs :cl-gena/random)
  (:export #:bit-array-genotype
           #:get-sequence
           #:bag-population 
           #:bit-array-genotype-generator)
  )

(in-package :cl-gena/bit-array-genotype)

(defclass bit-array-genotype (genotype)
  ((sequence :reader get-sequence
             :initarg :sequence
             :initform #*
             :type simple-bit-vector)
   (size :reader size
         :initarg :size)))

(defclass bag-population (population) ())

(defmethod print-object ((g bit-array-genotype) s)
  (format s "#<BAG ~A>" (get-sequence g)))

(defmethod print-object ((p bag-population) s)
  (format s "#<BAG-POPULATION ~A [" (size p))
  (loop for g in (genotype-list p)
     do (format s "~%    ~A" g))
  (format s " ]>"))

(defun bit-array-genotype-generator (size &key (type 'bit-array-genotype))
  (lambda (index)
    (declare (ignore index))
    (make-instance type
                   :sequence (loop with array = (make-array size :element-type 'bit)
                                for i from 0 below size
                                do (setf (bit array i)
                                         (random 2))
                                finally (return array))
                   :size size)))

(defmethod mutate ((g bit-array-genotype))
  (let* ((s (copy-seq (get-sequence g)))
         (max-n (round (* (size g) *mutation-width*)))
         (n (if (= 0 max-n) 1 (1+ (random max-n))))
         (indexes (random-indexes n (size g)))) 
    (loop for i in indexes do
         (setf (bit s i)
               (mod (+ (bit s i) 1) 2)))
    (copy-instance g :sequence s)))

(defparameter *min-children* 1)
(defparameter *max-children* 2)

(defmethod crossover ((g1 bit-array-genotype) (g2 bit-array-genotype)) 
  (assert (= (size g1) (size g2)))
  (let ((size (size g1))
        (s1 (get-sequence g1))
        (s2 (get-sequence g2)))
    (labels ((%spawn-sequence ()
               (let* ((k1 (random-int :from 0 :to (- size 2)))
                      (k2 (random-int :from (1+ k1) :to (1- size)))
                      (child-seq (make-array size :element-type 'bit)))
                 (loop for i from 0 to k1
                    do (setf (bit child-seq i) (bit s1 i)))
                 (loop for i from (1+ k1) to k2
                    do (setf (bit child-seq i) (bit s2 i)))
                 (loop for i from (1+ k2) below size
                    do (setf (bit child-seq i) (bit s1 i)))
                 child-seq)))
      (loop repeat (random-int :from *min-children*
                               :to *max-children*)
         collect (copy-instance g1 :sequence (%spawn-sequence))))))


