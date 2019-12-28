

;; start Quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
	(when (probe-file quicklisp-init)
		(load quicklisp-init)))

;; Load Common Qt
(ql:quickload 'qt)

;; Define package
(defpackage :simple-gui
	(:use :cl :qt)
	(:export #:main))

(in-package :simple-gui)
(named-readtables:in-readtable :qt)

;; Load classes
(load "GUI/mainwindow.lsp")

;; Load AGA
(defvar *from-gui* "--gui")
(load "main.lisp")

;; Start the algorithm
(defmethod start-ga ((instance main-window))
	(#_setText (terminal instance) "")
	(loop for i from 0 below 100
			until (eq lowest 0.0)
			do (update-output instance (generate-once i) i lowest 0)))

;;; Main program
(let* ((app (make-qapplication))
       (window (make-instance 'main-window)))

	(#_show window)

	(unwind-protect
		(#_exec app)
	(#_hide window)))

