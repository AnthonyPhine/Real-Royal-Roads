;;; This file is for all the functions relating to dealing
;;; with the genome. Including encoding, decoding and printing

;; this model holds no difference between geno and pheno

;; Convert the genotype to it's float value
(defun phenotype (genotype)
	genotype)

;; Prett print function for final output, maybe print traps?
(defun pretty (lst)
	(let ((out "["))
		(loop for i from 0 below (length lst) do
			(setq out (concatenate 'string out (write-to-string (nth i lst)))))
		(concatenate 'string out "]")))
