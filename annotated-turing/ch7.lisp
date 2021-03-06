;;; This is a Turing Machine implementation for running the examples in chapter
;;; 7 of the book The Annotated Turing (http://www.theannotatedturing.com/) by
;;; Charles Petzold.

;; Tape
(let ((tape (make-hash-table))
      (head 0)
      (min 0)
      (max 0))

  (defun make-turing-machine (&key starting-state)
    (let ((next-configuration starting-state)
	  (step 0))
      (clear-tape)
      (defun next (&optional (n 1))
	(dotimes (i n) 
	  (if (consp next-configuration)
	      (if (cdr next-configuration)
		  (setf next-configuration (funcall (car next-configuration) (cadr next-configuration)))
		  (setf next-configuration (funcall (car next-configuration))))
	      (setf next-configuration (funcall next-configuration)))
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

  (defun N ()
    "No move")
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
			 (cond ((eql (car state) 'none)
				`((eql (read-symbol) nil)
				  ,@(cadr state)
				  ',(caddr state)))
			       ((eql (car state) 'else)
				`(t ,@(cadr state)
				    ',(caddr state)))
			       (t `((eql (read-symbol) ',(car state))
				    ,@(cadr state)
				    ',(caddr state)))))
		     states))))

;; This macro expands an m-function list into a defun
;; NOTE! m-functions' intermediate states aren't printed by (next)
(defmacro m-function (name args &body states)
  `(defun ,name ,args
     (cond ,@(mapcar #'(lambda (state)
			 (let ((final-config (if (or (member (caddr state) args)
						     (listp (caddr state)))
						 (caddr state)
						 `',(caddr state))))
			   (cond ((eql (car state) 'none)
				  `((eql (read-symbol) nil)
				    ,@(cadr state)
				    ,final-config))
				 ((eql (car state) 'else)
				  `(t ,@(cadr state)
				      ,final-config))
				 (t `((eql (read-symbol) ',(car state))
				      ,@(cadr state)
				      ,final-config)))))
		     states))))
		      
;;; Here are the example Turing machine configurations from the book.

;; This is not really a new Turing machine but rather the one that calculates
;; the square root of 2 (p102) from ch6.lisp when using m-functions instead.
(defmacro p111 ()
  `(progn
     (m-configuration new     ; On p112 Charles' mentions that this m-function
       (@ ((R)) mark-digits)  ; together with new-digit-is-zero and new-digit-is-one
       (else ((L)) new))      ; can be removed. I have added it again because it
                              ; is used by the m-configuration cleanup.
     (m-function goto-sentinel (A)
       (@ ((R)) A)
       (else ((L)) (goto-sentinel A)))
     (m-configuration begin
       (none ((P '@) (R) (P 1)) (goto-sentinel mark-digits)))
     (m-configuration mark-digits
       (0 ((R) (P 'x) (R)) mark-digits)
       (1 ((R) (P 'x) (R)) mark-digits)
       (none ((R) (P 'z) (R) (R) (P 'r)) find-x))
     (m-configuration find-x
       (x ((E)) first-r)
       (@ ((N)) find-digits)
       (else ((L) (L)) find-x))
     (m-configuration first-r
       (r ((R) (R)) last-r)
       (else ((R) (R)) first-r))
     (m-configuration last-r
       (r ((R) (R)) last-r)
       (none ((P 'r) (R) (R) (P 'r)) find-x))
     (m-configuration find-digits
       (@ ((R) (R)) find-1st-digit)
       (else ((L) (L)) find-digits))
     (m-configuration find-1st-digit
       (x ((L)) found-1st-digit)
       (y ((L)) found-1st-digit)
       (z ((L)) found-2nd-digit)
       (none ((R) (R)) find-1st-digit))
     (m-configuration found-1st-digit
       (0 ((R)) add-zero)
       (1 ((R) (R) (R)) find-2nd-digit))
     (m-configuration find-2nd-digit
       (x ((L)) found-2nd-digit)
       (y ((L)) found-2nd-digit)
       (none ((R) (R)) find-2nd-digit))
     (m-configuration found-2nd-digit
       (0 ((R)) add-zero)
       (1 ((R)) add-one)
       (none ((R)) add-one))
     (m-configuration add-zero
       (r ((P 's)) add-finished)
       (u ((P 'v)) add-finished)
       (else ((R) (R)) add-zero))
     (m-configuration add-one
       (r ((P 'v)) add-finished)
       (u ((P 's) (R) (R)) carry)
       (else ((R) (R)) add-one))
     (m-configuration carry
       (r ((P 'u)) add-finished)
       (none ((P 'u)) (goto-sentinel (print-digit 0)))
       (u ((P 'r) (R) (R)) carry))
     (m-configuration add-finished
       (@ ((R) (R)) erase-old-x)
       (else ((L) (L)) add-finished))
     (m-configuration erase-old-x
       (x ((E) (L) (L)) print-new-x)
       (z ((P 'y) (L) (L)) print-new-x)
       (else ((R) (R)) erase-old-x))
     (m-configuration print-new-x
       (@ ((R) (R)) erase-old-y)
       (y ((P 'z)) find-digits)
       (none ((P 'x)) find-digits))
     (m-configuration erase-old-y
       (y ((E) (L) (L)) print-new-y)
       (else ((R) (R)) erase-old-y))
     (m-configuration print-new-y
       (@ ((R)) (goto-sentinel (print-digit 1)))
       (else ((P 'y) (R)) reset-new-x))
     (m-configuration reset-new-x
       (none ((R) (P 'x)) flag-result-digits)
       (else ((R) (R)) reset-new-x))
     (m-configuration flag-result-digits
       (s ((P 't) (R) (R)) unflag-result-digits)
       (v ((P 'w) (R) (R)) unflag-result-digits)
       (else ((R) (R)) flag-result-digits))
     (m-configuration unflag-result-digits
       (s ((P 'r) (R) (R)) unflag-result-digits)
       (v ((P 'u) (R) (R)) unflag-result-digits)
       (else ((N)) find-digits))
     (m-function print-digit (a)
       (0 ((R) (E) (R)) (print-digit a))
       (1 ((R) (E) (R)) (print-digit a))
       (none ((P a) (R) (R) (R)) cleanup))
     (m-configuration cleanup
       (none ((N)) new)
       (else ((E) (R) (R)) cleanup))

     (make-turing-machine :starting-state 'begin)))
