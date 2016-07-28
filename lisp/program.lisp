#!/usr/local/bin/clisp -C

(defun fact (n)
  (if (< n 2)
      1
      (* n (fact (1- n)))))

(defun program ()
  (loop
   (princ "fact: ")
   (let ((in (read)))
     (if (eql in 'q)
	 (return)
	 (if (numberp in)
	     (and (princ (fact in))
		  (princ #\Newline)))))))

(program)

;(fact 15000)

;(and (princ (fact 15000))
     (princ #\Newline))
