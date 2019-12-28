
(in-package :simple-gui)
(named-readtables:in-readtable :qt)

;; Class header
(defclass main-window ()
	((problem-type :accessor problem-type)
	 (select-type :accessor select-type)
	 (mutate-type :accessor mutate-type)
	 (best-solution :accessor best-solution)
	 (terminal :accessor terminal)
	 (gen-count :accessor gen-count)
	 (run-time :accessor run-time)
	 (run-butt :accessor run-butt))
	(:metaclass qt-class)
	(:qt-superclass "QMainWindow")
	(:slots ("startGA()" start-ga)))


;; Constructor
(defmethod initialize-instance :after ((instance main-window) &key)
	(new instance)
	(let ((box (#_new QGroupBox "Output"))
	      (main-lay (#_new QGridLayout))
	      (box-lay (#_new QGridLayout))
	      (main-widget (#_new QWidget)))
	(#_resize instance 700 400)
	(#_setWindowTitle instance "SimpleGUI")
	(setf (terminal instance) (#_new QTextEdit "Gen: (genotype) ==> phenotype = fitness")
	      (best-solution instance) (#_new QLabel "Best Solution: ")
	      (gen-count instance) (#_new QLabel "Gen:  ")
	      (run-time instance) (#_new QLabel "Runtime: 0")
	      (run-butt instance) (#_new QPushButton "Run"))

	; Connect
	(connect (run-butt instance) "clicked()" instance "startGA()")

	(#_addWidget box-lay (terminal instance))
	(#_setLayout box box-lay)
	(#_addWidget main-lay box 0 0 1 2)
	(#_addWidget main-lay (gen-count instance) 1 0 1 1 (#_Qt::AlignCenter))
	(#_addWidget main-lay (run-butt instance) 1 1 1 1 (#_Qt::AlignCenter))
	(#_addWidget main-lay (best-solution instance) 2 0 1 2 (#_Qt::AlignCenter))
	(#_setLayout main-widget main-lay)
	(#_setCentralWidget instance main-widget)))

;; Update method
(defmethod update-output ((instance main-window) out-text rtime generation best)
	(#_setText (terminal instance) (concatenate 'string (#_toPlainText (terminal instance)) (string #\linefeed) out-text))
	(#_setText (gen-count instance) (concatenate 'string "Gen: " (write-to-string generation)))
	(#_setText (best-solution instance) (concatenate 'string "Best: " (write-to-string generation)))
	(#_setText (run-time instance) (concatenate 'string "Runtime: " (write-to-string rtime)))

	(#_update instance))

