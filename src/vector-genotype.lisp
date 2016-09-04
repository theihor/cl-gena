(defpackage :cl-gena/vector-genotype
  (:use :common-lisp :cl-gena/fancy-tools :cl-gena/generic-defs :cl-gena/random))

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

(defmethod genotype-equal ((g1 vector-genotype) (g2 vector-genotype))
  (and (equalp (get-sequence g1)
               (get-sequence g2))
       (= (lower-bound g1)
          (lower-bound g2))
       (= (upper-bound g1)
          (upper-bound g2))
       (= (size g1)
          (size g2))))

(defun bounded+ (x y lb ub)
  (let ((z (+ x y)))
    (cond ((< z lb) lb)
          ((> z ub) ub)
          (t z))))

;; for the vector of reals let's have next mutation parameters
;; it is a good idea to decrease them during evolution
(defparameter *mutation-depth* 0.5)
(defparameter *mutation-width* 0.3)

(defmethod mutate ((g vector-genotype))
  (let* ((s (copy-seq (get-sequence g)))
         (n (round (* (size g) *mutation-width*)))
         (indexes (random-take (if (>= n 1) n 1) (loop for i from 0 to (1- (size g)) collect i)))) 
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
(defparameter *max-children* 4)

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

(defmethod fitness ((vg vector-genotype))
  "for y = sum(x ^ 2)"
  (/ 1.0
     (+ 1.0 (loop for x across (get-sequence vg) sum (* x x)))))

(defun best-fitness (pop)
  (loop for x across (get-sequence(maximum (genotype-list pop)
                                           :comparator #'fitness-comparator))
     sum (* x x)))

(defun test ()
  (setf *mutation-depth* 0.4)
  (setf *mutation-width* 0.3)
  (let ((pop (initialize-population vg-population
                                    (vector-genotype-generator 10 -5.0 +5.0)
                                    400))
        (max-i 200))
    ;; (format t "Initial pop: ~A~%" pop)
    (evolution pop
               :timeout 60.0
               :max-iteration max-i
               :step-func (lambda (pop i time)
                            (format t "~A [~A s]: ~E {mw = ~A; md = ~A;}~%"
                                    i (float time) (best-fitness pop) *mutation-width* *mutation-depth*)
                            ;; (format t "pop: ~A~%" pop)
                            (setf *mutation-depth* (* (/ (- max-i i) max-i) *mutation-depth*))
                            (setf *mutation-width* (* (/ (- max-i i) max-i) *mutation-width*))
                            ))))
