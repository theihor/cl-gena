(defpackage :cl-gena/vector-genotype
  (:use :common-lisp :cl-gena/fancy-tools :cl-gena/generic-defs :cl-gena/random)
  (:export #:vector-genotype
           #:vector-genotype-generator
           #:vg-population
           #:get-sequence))

(in-package :cl-gena/vector-genotype)

(defclass vector-genotype (genotype)
  ((sequence :reader get-sequence
             :initarg :sequence
             :initform #())
   (size :reader size
         :initarg :size) 
   (lower-bound :reader lower-bound
                :initarg :lower-bound)
   (upper-bound :reader upper-bound
                :initarg :upper-bound)))

(defclass vg-population (population) ())

(defmethod print-object ((g vector-genotype) s)
  (format s "#<VG [ ")
  (loop for x across (get-sequence g)
     do (format s "~E~T" x))
  (format s "]>"))

(defmethod print-object ((p vg-population) s)
  (format s "#<VG-POPULATION ~A [" (size p))
  (loop for g in (genotype-list p)
     do (format s "~%    ~A" g))
  (format s " ]>"))

(defun vector-genotype-generator (size lower-bound upper-bound &key (type 'vector-genotype))
  (lambda (index)
    (declare (ignore index))
    (make-instance type
                   :sequence (loop with array = (make-array size)
                                for i from 0 below size
                                do (setf (aref array i)
                                         (random-float :from lower-bound
                                                       :to upper-bound))
                                finally (return array))
                   :size size
                   :lower-bound lower-bound
                   :upper-bound upper-bound)))

;; (defmethod genotype-equal ((g1 vector-genotype) (g2 vector-genotype))
;;   (and (equalp (get-sequence g1)
;;                (get-sequence g2))
;;        (= (lower-bound g1)
;;           (lower-bound g2))
;;        (= (upper-bound g1)
;;           (upper-bound g2))
;;        (= (size g1)
;;           (size g2))))

(defun bounded+ (x y lb ub)
  (let ((z (+ x y)))
    (cond ((< z lb) lb)
          ((> z ub) ub)
          (t z))))

(defmethod mutate ((g vector-genotype))
  (let* ((s (copy-seq (get-sequence g)))
         (max-n (round (* (size g) *mutation-width*)))
         (n (if (= 0 max-n) 1 (1+ (random max-n)))) 
         (indexes (random-indexes n (size g)))) 
    (loop for i in indexes do
         (setf (aref s i)
               (bounded+ (aref s i) 
                         (* (if (probability-check 0.5) -1.0 1.0)
                            *mutation-depth*
                            (random-float :from (lower-bound g)
                                          :to (upper-bound g)))
                         (lower-bound g)
                         (upper-bound g))))
    (copy-instance g :sequence s)))

(defparameter *min-children* 1)
(defparameter *max-children* 2)

(defmethod crossover ((g1 vector-genotype) (g2 vector-genotype)) 
  (assert (= (size g1) (size g2)))
  (let ((size (size g1))
        (s1 (get-sequence g1))
        (s2 (get-sequence g2)))
    (labels ((%spawn-sequence ()
               (let* ((k1 (random-int :from 0 :to (- size 2)))
                      (k2 (random-int :from (1+ k1) :to (1- size)))
                      (child-seq (make-array size)))
                 (loop for i from 0 to k1
                    do (setf (aref child-seq i) (aref s1 i)))
                 (loop for i from (1+ k1) to k2
                    do (setf (aref child-seq i) (aref s2 i)))
                 (loop for i from (1+ k2) below size
                    do (setf (aref child-seq i) (aref s1 i)))
                 child-seq)))
      (loop repeat (random-int :from *min-children*
                               :to *max-children*)
         collect (copy-instance g1
                                :sequence (%spawn-sequence))))))


