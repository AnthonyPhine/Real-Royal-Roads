
(load "genetic-operations.lisp")
(load "royal-road.lisp")
(load "encoding.lisp")

;; Create a new random population
(defun generate-pop (pop-size genome-size)
	(loop for i from 0 below pop-size
		collect (loop for j from 0 below genome-size
			collect (nth (random 2) '(0 1)))))
			
;; == Really nice macro for pop-nth ==
(defun remove-nth (list n)
  (remove-if (constantly t) list :start n :end (1+ n)))

(define-modify-macro remove-nth-f (n) 
	remove-nth "Remove the nth element")

(defmacro pop-nth (n lst)
  (let ((n-var (gensym)))
    `(let ((,n-var ,n))
       (prog1 (nth ,n-var ,lst)
         (remove-nth-f ,lst ,n-var)))))
;; ====================================	

;; Selection (tournament - dueling)
(defun selection (population amount)
	(if (eq amount 1)
		(nth (random (length population)) population)
		(let ((outpop nil) index-one index-two genome-one genome-two)
			(loop for i from 0 below amount
				do (progn
					(setq index-one (random (length population)))
					(setq genome-one (pop-nth index-one population))
					(setq index-two (random (length population)))
					(setq genome-two (pop-nth index-two population))
					(if (> (fitness (phenotype genome-one))
					       (fitness (phenotype genome-two)))
						(setq outpop (cons genome-one outpop))
						(setq outpop (cons genome-two outpop)))))
			outpop)))

;; returns a tuple (best-fitness . worst-fitness)
(defun pop-fitness-limits (population)
	(let ((best 0.0) (pls 1.0))
		(loop for genotype in population do
			(let ((score (fitness genotype)))
				(if (> score best)
					(setq best score))
				(if (< score pls)
					(setq pls score))))
		(list best pls)))

(defun get-most-worst (countmap)
	(let* ((index (random (length countmap)))
	       (initial (nth index countmap))
	       (failure (nth 0 initial))
	       (amount 0))
		(loop for mp in countmap do
			(if (> (nth 1 mp) amount)
				(progn
					(setq failure (nth 0 mp))
					(setq amount (nth 1 mp)))))
		(if (null failure)
		(progn
			(format t "~a~%" countmap) (quit)))
		failure))

(defun increment-map (individual countmap)
	(let ((add-to-map t))
		(loop for mp in countmap do
			(if (equal (nth 0 mp) individual)
				(progn
					(setq add-to-map nil)
					(setf (nth 1 mp) (1+ (nth 1 mp))))))
		(if (null countmap)
			(list (list individual 0))
			(append countmap (list (list individual 0))))))

;; removes one individual with fitness lower than the bar
;; which has the most copies of itself in the population
(defun kill-weakest (bar population)
	(let ((countmap nil))
		(loop for genotype in population do
			(if (<= (fitness genotype) bar)
				(setq countmap (increment-map genotype countmap))))
		(remove (get-most-worst countmap) population)))

;; A function  that removes the lowest fitness indivual that has
;; the most copies in the population a new individual
;; returns the population if fitness is to low
(defun stabilise-drift (individual population)
	(let* ((limits (pop-fitness-limits population))
	       (score (fitness individual))
	       (top (nth 0 limits))
		   (bottom (nth 1 limits)))
		(if (< score bottom)
			population
			(kill-weakest score (append (list individual) population)))))

;; Function to apply crossover to 
;; the whole population
(defun pop-crossover (selected-pop size)
	(let* ((pop-length (length selected-pop))
	      (out
	       	(loop for i from (length selected-pop) below size collect
				(crossover
					(nth (random pop-length) selected-pop)
					(nth (random pop-length) selected-pop)))))
		(append selected-pop out)))

;; Function to apply mutation to
;; the whole population
(defun pop-mutate (population rate)
	(let ((out population))
		(setf out (loop for i from 0 below (length out)
			collect (mutate (nth i out) rate)))))

;; =============================================================================

;; Roulette-wheel Selection
(defun roulette (group)
	(let* ((individual (nth (random (length group)) group))
	       (ball (random (float (loop for i in group sum (fitness i)))))
	       (tally 0) (result nil))
		(loop for genome in group do
			(progn
				(setq tally (+ tally (fitness genome)))
				(if (> tally ball)
					(setq result genome)))
			until (not (null result)))
		result))


;; Boltzmann Selection
(defun boltzmann (group &optional (tally 0))
	(let ((result nil) (max-prob 0) (size (length group)))
	
		(setq max-prob (loop for genome in group sum
		                     (exp (/ (fitness genome) size))))

		(loop for genome in group do
			(progn
				(setq tally (+ tally (/ (exp (/ (fitness genome) size)) max-prob)))
				(if (< (random max-prob) tally)
					(setq result genome)))
			until (not (null result)))

		(if (null result)
			(boltzmann group tally)
			result)))


(defun unique-append (item list)
	(if (not (member item list))
		(cons item list)
		list))

;; Tournament Selection for n-sized tournaments
(defun tournament (group &optional (brawl-size 2))
	(let ((fighters nil) (winner nil))
		(loop until (eq (length fighters) brawl-size) do
			(setq fighters (unique-append
				(nth (random (length group)) group) fighters)))

		(setq winner (nth 0 fighters))
		(loop for fighter in fighters do
			(if (> (fitness fighter) (fitness winner))
				(setq winner fighter)))
		winner))

(defvar *selection-type* nil)
(defun is-select (type) (if (equal type *selection-type*) t))
(defun select-by-type (group)
	(cond
		((is-select "Tournament") (tournament group))
		((is-select "Boltzmann")  (boltzmann group))
		((is-select "Roulette")   (roulette group))
		(t (nth (random (length group)) group))))

;; A generalised selection
(defun selection (population amount)
	(if (eq amount 1)
		(select-by-type population)
		(loop for i from 0 below amount collect
			(select-by-type population))))

