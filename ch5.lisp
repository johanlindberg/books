;; This is a Turing Machine implementation for running the examples in
;; the book The Annotated Turing (http://www.theannotatedturing.com/)
;; by Charles Petzold.

;; Tape
(let ((tape (make-hash-table))
      (head 0)
      (max 0))

  (defun make-turing-machine (&key starting-state)
    (let ((next-configuration starting-state)
	  (step 0))
      (defun next ()
	(setf next-configuration (funcall next-configuration))
	(incf step)
	(print-tape step next-configuration))))

  (defun clear-tape ()
    (setf tape (make-hash-table))
    (setf head 0)
    (setf max 0))

  (defun print-tape (step m-conf)
    (let ((_tape (make-string (+ max 1) :initial-element #\_))
	  (_head (make-string (+ head 1) :initial-element #\SPACE)))
      (setf (char _head head) #\^)
      (maphash #'(lambda (key val)
		   (setf (char _tape key) (character val)))
	       tape)
      (format t "~&TAPE: ~A  NEXT: ~A~%      ~A  STEP: ~D~%" _tape m-conf _head step)))


  ; Tape operations
  (defun read-symbol ()
    (gethash head tape))

  (defun write-symbol (arg)
    (setf (gethash head tape) (write-to-string arg)))

  (defun move-left ()
    (decf head))
      
  (defun move-right ()	 
    (incf head)
    (when (> head max)
      (setf max head))))

;; M-configurations
(defmacro m-configuration (name &rest states)
  `(defun ,name ()
     (cond ,@(mapcar #'(lambda (state)
			 `((eq (read-symbol) ',(car state))
			   ,@(cadr state)
			   ',(caddr state)))
		     states))))

(defmacro P (arg)
  `(write-symbol ,arg))

(defmacro R ()
  `(move-right))

(defmacro L ()
  `(move-left))


(defmacro p81 ()
  `(progn
     (m-configuration b (nil ((P 0) (R)) c))
     (m-configuration c (nil ((R)) e))
     (m-configuration e (nil ((P 1) (R)) f))
     (m-configuration f (nil ((R)) b))

     (make-turing-machine :starting-state 'b)))
