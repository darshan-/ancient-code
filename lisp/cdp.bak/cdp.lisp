;;;#!/usr/local/bin/clisp -K full
;;;sbcl --noinform --noprint --userinit cdp.lisp

(defvar action)

(load "cdp-actions.lisp")

(defun timep (str)
  "returns t iff str is a valid time"
  (cond ((not (stringp str)) nil)
	((not (find #\: str)) nil)
	((and (= (- (length str) (position #\: str)) 3)
	      (>= (length str) 4)   ; "0:00"
	      (<= (length str) 5))  ; "99:59"
	 t)
	(t nil)))

(defun destring (list)
  "takes a list of strings;  returns list of symbols;
   times remain strings in new list."
  (if (not (listp list))
      (error "when is a list not a list?"))
  (let ((new-list nil))
    (dolist (element list)
      (if (find #\: element)
	  (if (timep element)
	      (push element new-list)
	      (progn
		(format t "cdp.lisp: ~S: bad time~%" element)
		(quit)))
	  (push (read-from-string element) new-list)))
    (reverse new-list)))

(setf *posix-argv* (destring (cdr *posix-argv*)))

(defparameter *commands*
  '((p . play-pause)
    (play . play)
    (pause . pause)
    (s . stop)
    (f . forward)
    (ff . fforward)
    (b . backward)
    (bb . bbackward)
    (+ . seekf)
    (- . seekb)
    (o . tray-open)
    (c . tray-close)
    (e . tray-open)			;e for eject
    (i . info)
    (l . play-list)))

(setf action (cdr (assoc (car *posix-argv*) *commands*)))

(if (numberp (car *posix-argv*)) (setf action 'play-track))
(if (stringp (car *posix-argv*)) (setf action 'seek-time))
(if (null *posix-argv*) (setf action 'status))

(if (null action)
    (progn
      (format t "cdp.lisp: ~S: unrecognized command~%" (car *posix-argv*))
      (quit)))

(funcall action)

(quit)
