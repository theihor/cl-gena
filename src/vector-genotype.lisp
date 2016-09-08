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
(defparameter *initial-mutation-depth* 1e-3)
(defparameter *mutation-depth* *initial-mutation-depth*)
(defparameter *initial-mutation-width* 0.2)
(defparameter *mutation-width* *initial-mutation-width*)

(defmethod mutate ((g vector-genotype))
  (let* ((s (copy-seq (get-sequence g)))
         (n (round (* (size g) *mutation-width*)))
         (n (if (= 0 n) 1 n))
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
(defparameter *max-children* 1)

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

(defmacro sqr (x)
  (let ((z (gensym)))
    `(let ((,z ,x))
       (* ,z ,z))))

(defun decrease-mutation-round (i max-i)
  (setf *mutation-depth* (* (sqrt (- 1 (sqr (/ i max-i)))) *initial-mutation-depth*))
  (setf *mutation-width* (* (sqrt (- 1 (sqr (/ i max-i)))) *initial-mutation-width*)))

(defun decrease-mutation-linear (i max-i)
  (setf *mutation-depth* (* (/ (- max-i i) max-i) *initial-mutation-depth*))
  (setf *mutation-width* (* (/ (- max-i i) max-i) *initial-mutation-width*)))

(defun decrease-mutation-exp (i max-i)
  (setf *mutation-depth* (* (exp (/ (- i) max-i)) *initial-mutation-depth*))
  (setf *mutation-width* (* (exp (/ (- i) max-i)) *initial-mutation-width*)))

(defun test ()
  ;; (setf *initial-mutation-width* 0.4)
  ;; (setf *initial-mutation-depth* 0.3)
  (let* ((pop (initialize-population vg-population
                                     (vector-genotype-generator 20 -1000.0 +1000.0)
                                     400))
         (max-i 400)
         (b (best-fitness pop)))
    ;; (format t "Initial pop: ~A~%" pop)
    (evolution pop
               :timeout 60.0
               :max-iteration max-i
               :step-func (lambda (pop i time)
                            (let ((new-b (best-fitness pop)))
                              (cond ((> b new-b)
                                     (setf b new-b)
                                     (format t "~A [~A s]: ~1,2E {mw = ~A; md = ~A;}~%"
                                             i (float time) b *mutation-width* *mutation-depth*))
                                    ((< b new-b) 
                                     (format t "WORSING ~A [~A s]: ~E {mw = ~A; md = ~A;}~%"
                                             i (float time) new-b *mutation-width* *mutation-depth*))
                                    (t nil)))
                            ;; (format t "pop: ~A~%" pop)
                            
                            (decrease-mutation-linear i max-i)
                            ))))
