;;; This is a Turing Machine implementation for running the examples in chapter
;;; 6 of the book The Annotated Turing (http://www.theannotatedturing.com/) by
;;; Charles Petzold.

;; Tape
(let ((tape (make-hash-table))
      (head 0)
      (min 0)
      (max 0))

  (defun head-pos () head)

  (defun make-turing-machine (&key starting-state)
    (let ((next-configuration starting-state)
	  (step 0))
      (clear-tape)
      (defun fake-step (arg)
	(setf next-configuration arg)
	(incf step)
	(print-tape :step step :next-conf next-configuration))
      (defun next (&optional (n 1))
	(dotimes (i n) 
	  (setf next-configuration (funcall next-configuration))
	  (incf step))
	(print-tape :step step :next-conf next-configuration))))

  (defun clear-tape ()
    (setf tape (make-hash-table))
    (setf head 0)
    (setf min 0)
    (setf max 0))

  (defun print-tape (&key step next-conf)
    (let ((_tape (make-string (+ (abs min) max 1) :initial-element #\_))
	  (_head (make-string (+ (abs min) max 1) :initial-element #\SPACE)))
      (setf (char _head (+ (abs min) head)) #\^)
      (maphash #'(lambda (key val)
		   (setf (char _tape (+ (abs min) key)) (character (write-to-string val))))
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
    (decf head)
    (when (< head min)
      (setf min head))))

      
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

;; Computes all positive integers in sequence 0, 1, 10, 11, 100, ..
(defmacro p99 ()
  `(progn
     (m-configuration begin
       (nil ((P 0)) increment))
     (m-configuration increment
       (0 ((P 1)) rewind)
       (1 ((P 0) (L)) increment)
       (nil ((P 1)) rewind))
     (m-configuration rewind
       (nil ((L)) increment)
       (0 ((R)) rewind)
       (1 ((R)) rewind))

     (make-turing-machine :starting-state 'begin)))

