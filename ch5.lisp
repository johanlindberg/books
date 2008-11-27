;;; This is a Turing Machine implementation for running the examples in chapter
;;; 5 of the book The Annotated Turing (http://www.theannotatedturing.com/) by
;;; Charles Petzold.

;; Tape
(let ((tape (make-hash-table))
      (head 0)
      (max 0))

  (defun make-turing-machine (&key starting-state)
    (let ((next-configuration starting-state)
	  (step 0))
      (clear-tape)
      (defun next (&optional (n 1))
	(dotimes (i n) 
	  (setf next-configuration (funcall next-configuration))
	  (incf step))
	(print-tape :step step :next-conf next-configuration))))

  (defun clear-tape ()
    (setf tape (make-hash-table))
    (setf head 0)
    (setf max 0))

  (defun print-tape (&key step next-conf)
    (let ((_tape (make-string (+ max 1) :initial-element #\_))
	  (_head (make-string (+ max 1) :initial-element #\SPACE)))
      (setf (char _head head) #\^)
      (maphash #'(lambda (key val)
		   (setf (char _tape key) (character (write-to-string val))))
	       tape)
      (format t "~&TAPE: ~A  NEXT: ~A~%      ~A  STEP: ~D~%" _tape next-conf _head step)))

  ;; Available tape operations
  (defun read-symbol ()
    (gethash head tape))

  ;; These are the available operations that can be used
  ;; in the m-configurations.
  (defun P (arg)
    "Print"
    (setf (gethash head tape) arg))
  (defun E ()
    "Erase"
    (remhash head tape))
  (defun R ()
    "Move right"
    (incf head)
    (when (> head max)
      (setf max head)))
  (defun L ()
    "Move left"
    (decf head)))
      
;; This macro expands an m-configuration list into a defun
(defmacro m-configuration (name &body states)
  `(defun ,name ()
     (cond ,@(mapcar #'(lambda (state)
			 `((eql (read-symbol) ',(car state))
			   ,@(cadr state)
			   ',(caddr state)))
		     states))))

;;; Here are the example Turing machine configurations from the book. I've
;;; prefixed states with "." in order to avoid name clashes with tape operations.
;;; In the book, and in Turing's paper, this is done by using different fonts.

;; Computes 010101010101010101010...
(defmacro p81 ()
  `(progn
     (m-configuration .b (nil ((P 0) (R)) .c))
     (m-configuration .c (nil ((R)) .e))
     (m-configuration .e (nil ((P 1) (R)) .f))
     (m-configuration .f (nil ((R)) .b))

     (make-turing-machine :starting-state '.b)))

(defmacro p84 ()
  `(progn
     (m-configuration .b
       (nil ((P 0)) .b)
       (0 ((R) (R) (P 1)) .b)
       (1 ((R) (R) (P 0)) .b))

     (make-turing-machine :starting-state '.b)))

;; Computes 010000000000000000000...
(defmacro p83 ()
  `(progn
     (m-configuration .b (nil ((P 0) (R)) .c))
     (m-configuration .c (nil ((R)) .d))
     (m-configuration .d (nil ((P 1) (R)) .e))
     (m-configuration .e (nil ((R)) .f))
     (m-configuration .f (nil ((P 0) (R)) .e))

     (make-turing-machine :starting-state '.b)))

;; Computes 001011011101111011111...
(defmacro p87 ()
  `(progn
     (m-configuration .b
       (nil ((P '@) (R) (P '@) (R) (P 0) (R) (R) (P 0) (L) (L)) .o))
     (m-configuration .o
       (1 ((R) (P 'x) (L) (L) (L)) .o)
       (0 () .q))
     (m-configuration .q
       (0 ((R) (R)) .q)
       (1 ((R) (R)) .q)
       (nil ((P 1) (L)) .p))
     (m-configuration .p
       (x ((E) (R)) .q)
       (@ ((R)) .f)
       (nil ((L) (L)) .p))
     (m-configuration .f
       (0 ((R) (R)) .f)
       (1 ((R) (R)) .f)
       (nil ((P 0) (L) (L)) .o))

     (make-turing-machine :starting-state '.b)))
