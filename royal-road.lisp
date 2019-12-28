
;; count the consecutive 1s in the bitstring and
;; return the length of the largest block
(defun bit-bonus (genotype)
	(let ((longest 0) (current 0))
		(loop for i in genotype do
			(progn
				(if (eq i 0)
					(setq current 0))
				(if (eq i 1)
					(progn
						(setq current (1+ current))
						(setq longest (max current longest))))))
		longest))

;; return the amount of 1s in a bitstring
(defun unitation (genotype)
	(loop for i in genotype sum i))

;; as defined in the paper
(defun royal-road (genotype blocksize)
	(let ((n (length genotype)) (m blocksize)
		  (bonus (bit-bonus genotype))
		  (units (unitation genotype)))
		(if (eq n units)
			(* 2 (expt n 2))
			(if (<= units (- n m))
				(+ (* n units) bonus)
				0))))

;(defun demo (genotype blocksize)
;	(format t "g := ~a   f(g) = ~a~%"
;	        genotype (royal-road genotype blocksize)))

;(demo '(0 0 0 0 0 0 0 0) 4)
;(demo '(0 0 0 0 0 0 0 1) 4)
;(demo '(0 0 0 0 0 0 1 1) 4)
;(demo '(0 0 0 0 0 1 1 1) 4)
;(demo '(0 0 0 0 1 1 1 1) 4)
;(demo '(0 0 0 1 1 1 1 1) 4)
;(demo '(0 0 1 1 1 1 1 1) 4)
;(demo '(0 1 1 1 1 1 1 1) 4)
;(demo '(1 1 1 1 1 1 1 1) 4)
;(demo '(1 0 1 0 0 0 1 1) 4)
