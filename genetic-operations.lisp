;;; ============================================ ;;;
;;; Selection, Mutation, and Crossover Functions ;;;
;;; ============================================ ;;;

;; Nice and random
(setf *random-state* (make-random-state t))

;; Simple Flip-Bit method
(defun mutate (bit-list rate)
	(let ((val (random 1.0))
	      (genome (copy-list bit-list))
	      (bitcount (length bit-list)))
		(loop for i from 0 below bitcount
			do (let ((index (random bitcount)))
				(if (> val rate)
					(setf (nth index genome)
						(if (eql (nth index genome) 0)
							1
							0)))))
		genome))


;; Simple N-point crossover
;; Defaults as 2-point crossover
(defun crossover (parents)
	(let* ((cindex (random 2)) ; crossover index of the parent
	       (one (nth cindex parents))
	       (two (nth (- 1 cindex) parents))
	       (kpoint (random (length one)))
	       (start (loop for i from 0 to (1- kpoint) collect (nth i one)))
		   (end (loop for i from kpoint to (1- (length one)) collect (nth i two))))
		(append start end)))

;(format t "A: ~a~%B: ~a~%C: ~a~%~%"
;        '(1 1 1 1 1 1 1 1 1 1 1 1)
;        '(0 0 0 0 0 0 0 0 0 0 0 0)
;        (crossover '(1 1 1 1 1 1 1 1 1 1 1 1) '(0 0 0 0 0 0 0 0 0 0 0 0)))
