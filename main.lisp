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

;; Will recursively loop until max ones is found
(defun run-algorithm (current-pop gen-count)
	(let ((parents nil) (child nil) (solutions nil)
	      (best '(0)) (cross-index (random 2)))

		(if (> (random 1.0) *crossover-rate*)
			(progn
				(setq parents (selection current-pop 2))
				(setq child (crossover (nth cross-index parents)
				                       (nth (- 1 cross-index) parents))))
				(setq child (nth (random (length current-pop)) current-pop)))

		(setq child (mutate child *mutation-rate*))
		
		(setq current-pop (stabilise-drift child current-pop))
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

;; idealised single run version of the above
;(defun run-algorithm (population)
;	(let ((parents nil) (child nil))
;		(if (> (random 1.0) *crossover-rate*)
;			(setq child (crossover (selection current-pop 2)))
;			(setq child (selection current-pop 1)))
;		(setq child (mutate (child *mutation-rate*)))
;		(stabilise-drift current-pop child)))


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
