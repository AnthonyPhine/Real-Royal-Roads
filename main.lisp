;;; ========================================================= ;;;
;;  This is a Genetic Algorithm for finding the max ones
;;; ========================================================= ;;;

(load "population.lisp")

(defun cmd-error ()
	(format t "arguments must be in the form:~%~%     main.lisp [chromosome length] [inner block length]~%~%")
	(quit))

(defun get-arg (n)
	(parse-integer (nth n (cdr *posix-argv*)) :junk-allowed t))

(defun validate-cmd-args ()
	(if (not (eq 2 (length (cdr *posix-argv*))))
		(cmd-error))
	(if (null (get-arg 0))
		(cmd-error))
	(if (null (get-arg 1))
		(cmd-error)))

(validate-cmd-args)

;; Set up environment from arguments
(defvar *genotype-length* (get-arg 0))
(defvar *block-size* (get-arg 1))
(defvar *population-size* *genotype-length*)
(defvar *verbose* nil)
(defvar *crossover-rate* 0.5)
(defvar *mutation-rate* (/ 1 *genotype-length*))
(defvar *initial-pop* (generate-pop *population-size* *genotype-length*))

;; The fitness function
(defun fitness (genotype)
	(let ((fitness-limit (* 2 (expt *genotype-length* 2))))
		(/ (royal-road genotype *block-size*) fitness-limit)))


;; This is the heart of the genetic algorithm
(defun evolve (population)
	(let ((child nil) (next-pop nil))
		(if (> (random 1.0) *crossover-rate*)
			(setq child (crossover (selection population 2)))
			(setq child (selection population 1)))
		(setq child (mutate child *mutation-rate*))
		(setq next-pop (stabilise-drift child population))
		(if (equal next-pop population)
			(evolve population)
			next-pop)))


;; Will recursively loop until max ones is found
(defun run-algorithm (current-pop gen-count)
	(let ((solutions nil) (best '(0)))

		;; evolve the population
		(setq current-pop (evolve current-pop))

		;; measure health of population
		(loop for genome in current-pop do
			(let ((score (fitness genome)))
				(if (> score (fitness best))
					(setq best genome))
				(if (eq score 1)
					(setq solutions (cons genome solutions)))))

		(if *verbose*
			(format t "Best genome: ~a (cycle: ~a)~%" (pretty best) gen-count))

		(if (and (null solutions) (< gen-count 1000000))
			(run-algorithm current-pop (1+ gen-count))
			(let ((countmap nil))
				(if *verbose*
					(format t "Solutions: ~a~%" solutions))
				gen-count))))


;;;; ------------------------------------------ ;;;;
;;; Main program to be run if using command line ;;;
;;;; ------------------------------------------ ;;;;

;; Check to see if this is a gui invocation
(defvar *from-gui* (if (boundp '*from-gui*) *from-gui* nil))

(defun main ()
	(let ((result (run-algorithm *initial-pop* 0)))
		(if *verbose*
			(format t "~%Algorithm completed in ~a cycles (R_~a,~a)~%~%"
			          result *genotype-length* *block-size*)
			(format t "~a" result))))

;; The command line version
(if (not (equal *from-gui* "--gui"))
	(main))
