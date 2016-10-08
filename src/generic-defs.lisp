(defpackage :cl-gena/generic-defs
  (:use :common-lisp :anaphora :cl-gena/fancy-tools :cl-gena/random)
  (:export #:genotype #:genotype-equal
           #:population #:size #:genotype-list 
           #:fitness #:fitness-comparator
           #:initialize-population
           #:select
           #:crossover
           #:mutate
           #:terminate?
           #:evolution
           #:best-genotype
           #:*initial-mutation-depth*
           #:*mutation-depth*
           #:*initial-mutation-width*
           #:*mutation-width*
           #:*mutation-decrease-rate*
           #:*elitism-rate*
           #:*tournament-rate*
           ))

(in-package :cl-gena/generic-defs)

(defclass genotype () ())

(defclass population ()
  ((genotypes :accessor genotype-list
              :initarg :genotype-list
              :initform nil
              :type list)
   (size :reader size
         :initarg :size)))

(defmacro initialize-population (pop-type generator size)
  `(make-instance
    ',pop-type
    :size ,size
    :genotype-list (loop for i from 1 to ,size collect
                        (funcall ,generator i))))

(defgeneric genotype-equal (g1 g2))

(defmethod genotype-equal (g1 g2)
  (eq g1 g2))

(defparameter *fitness-cache* (make-hash-table :test #'eq))
;; Note: fitness is almost always problem specific
;; genotype -> number
(defgeneric fitness (g))

(defmethod fitness :around ((g genotype))
  ;; (when (> (hash-table-count *fitness-cache*) 10000)
  ;;   (clrhash *fitness-cache*)) 
  (aif (gethash g *fitness-cache*)
       it
       (let ((f (call-next-method)))
         (setf (gethash g *fitness-cache*) f))))

(defun fitness-comparator (g1 g2)
  (compare-number (fitness g1)
                  (fitness g2)))

(defgeneric clear-fitness-cache (population))

(defmethod clear-fitness-cache ((pop population))
  (with-slots (genotypes) pop
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (member k genotypes :test #'eq)
                 (remhash k *fitness-cache*)))
             *fitness-cache*)))

;; selection stage parameters
;; Note: the next equality must hold
;;          (n ^ 2 + n) * children-per-parents >= 2 * population-size
;;          where n = (elitism-rate + tournament-rate) * population-size (e. g. number of parents)
;;            and childern-per-parents is number of children generated by crossover from one pair
;;       otherwise the size of population will decrease in time
;; that's pretty easy though (to make population of size 1000 decay, (er + tr) should be < 0.032)

(defparameter *elitism-enabled* t)
(defparameter *elitism-rate* 0.14)

(defparameter *tournament-enabled* t)
(defparameter *tournament-t* 2)
(defparameter *tournament-rate* 0.3)

;; the probabilty that genotype pair will yield children
(defparameter *reproduction-probability* 1.0)

;; mutation depth is how much the value of particular gene may change
(defparameter *initial-mutation-depth* 1e-3)
(defparameter *mutation-depth* *initial-mutation-depth*)
;; mutation width is how many genes are affected by mutation
(defparameter *initial-mutation-width* 0.2)
(defparameter *mutation-width* *initial-mutation-width*)
;; it is good idea to decrease mutation over time
;; this parameter controls the decrease
;; may be :linear, :constant or :exponential
(defparameter *mutation-decrease-rate* :linear)

(defgeneric decrease-mutation (how i max-i))

(defmethod decrease-mutation ((how (eql :constant)) i max-i)
  (declare (ignore i max-i)))

(defmethod decrease-mutation ((how (eql :linear)) i max-i)
  (setf *mutation-depth*
        (* (/ (- max-i i) max-i)
           *initial-mutation-depth*))
  (setf *mutation-width*
        (* (/ (- max-i i) max-i)
           *initial-mutation-width*)))

(defmethod decrease-mutation ((how (eql :exponential)) i max-i)
    (setf *mutation-depth*
          (* (exp (/ (- i) max-i))
             *initial-mutation-depth*))
    (setf *mutation-width*
          (* (exp (/ (- i) max-i))
             *initial-mutation-width*)))

;; population -> ([genotype], [genotype])
(defgeneric select (population))

(defmethod select :before ((p population))
  (clear-fitness-cache p))

(defmethod select ((p population))
  "Default behaviour performs elitism and tournament"
  (let ((g-list (genotype-list p))
        elite champions)
    (when *elitism-enabled*
      (let ((elite-size (round (* *elitism-rate*
                                  (size p)))))
        (multiple-value-bind (best rest)
            (take-n-max elite-size g-list
                        :comparator #'fitness-comparator)
          (setf elite best)
          (setf g-list rest))))

    (when *tournament-enabled*
      (let ((champions-count (round (* *tournament-rate*
                                       (size p)))))
        (labels ((%remove-all (what from)
                   (reduce (lambda (lst x) (remove x lst))
                           what :initial-value from))
                 (%take (n lst &optional acc)
                   (if (or (<= n 0) (null lst))
                       acc
                       (let* ((candidates (random-take *tournament-t* lst))
                              (g (progn
                                   ;; (format t "candidates: ~A~%" candidates)
                                   (maximum candidates :comparator #'fitness-comparator))))
                         ;; (format t "winner: ~A~%" g)
                         (%take (1- n) (%remove-all candidates lst) (cons g acc))))))
          (setf champions (%take champions-count g-list)))))
    ;; (format t "elite: ~A~%champions: ~A~%" elite champions)
    (when (and (null elite)
               (null champions))
      (error "No genotype passed the selection stage"))
    (values elite champions)))

;; genotype -> genotype -> [genotype]
(defgeneric crossover (genotype1 genotype2))

;; genotype -> genotype
(defgeneric mutate (genotype))

(defgeneric terminate? (population))

(defmethod terminate? (pop)
  "Never terminate by default"
  (declare (ignore pop))
  nil)

;; population -> population
(defun evolve (pop)
  "Performs one iteration of evolution, spawning new population"
  (multiple-value-bind (elite champions) (select pop)
    (let* ((parents (append elite champions))
           (children (loop for p1 in parents append
                          (loop for p2 in parents
                             unless (eq p1 p2)
                             when (probability-check *reproduction-probability*)
                             append (mapcar #'mutate (crossover p1 p2)))))
           (genotype-list (append elite
                                  (take-n-max (- (size pop) (length elite))
                                              children
                                              :comparator #'fitness-comparator))))
      
      (copy-instance pop :genotype-list genotype-list))))

;; population -> population
(defun evolution (pop &key max-iteration timeout step-func)
  "Performs evloving of pop until one of the next condition is true:
      i > max-iteration
      time (specified by :timeout in seconds) elapsed
      (terminate? pop)

   After each step calls (if present)
     (step-func pop i time),
        where pop is current evolved pop
              i is current step number
              time is elapsed time from start in seconds

   NOTE: if you do not specify at least one stop condition, evolution will go forever"
  (let* ((i 0)
         (start-time (cl:get-internal-real-time))
         (current-time start-time)
         (*mutation-depth* *initial-mutation-depth*)
         (*mutation-width* *initial-mutation-width*))
    (loop until (or (and max-iteration (> i max-iteration))
                    (and timeout (> (- current-time start-time)
                                    (* cl:internal-time-units-per-second timeout)))
                    (terminate? pop))
       do (progn (setf pop (evolve pop))
                 (incf i)
                 (setf current-time (cl:get-internal-real-time))
                 (when step-func
                   (funcall step-func
                            pop i (/ (- current-time start-time)
                                     cl:internal-time-units-per-second)))
                 (or (when max-iteration
                       (decrease-mutation
                        *mutation-decrease-rate* i max-iteration)
                       t)
                     (when timeout
                       (decrease-mutation
                        *mutation-decrease-rate*
                        (- current-time start-time)
                        (* cl:internal-time-units-per-second timeout))
                       t)))))
  pop)

(defun best-genotype (pop)
  (maximum (genotype-list pop)
           :comparator #'fitness-comparator))

