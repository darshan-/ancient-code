;;;; cdp.lisp - main entry point


;; in case i'm loading this file in clisp or something
(defvar *posix-argv* ())

(defvar *action* nil "action to take")
(defvar *flags* () "flags set by command line options")
(defvar *command-data* () "command line data we still want")
(defvar *args* (cdr *posix-argv*))

;; these might help someone who's stuck
(defvar quit "type `(quit)' to exit")
(defvar exit quit)
(defvar help quit)


(defparameter *options*
  '((-i . interactive)
    (--interactive . interactive)
    (-q . quiet)
    (--quiet . quiet)))

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

(defparameter *takes-nums* '(seekf seekb info play-list play-track) "commands that allow numbers to follow them")
(defparameter *takes-times* '(seekf seekb seek-time) "commands that allow times to follow them")


(defstruct time
  (minute 0)
  (second 0)
  (frame 0))


(defun time-able (str)
  "returns t if and only if str is a valid time"
  (cond ((not (stringp str)) nil)
	((not (find #\: str)) nil)
	((and (= (- (length str) (position #\: str)) 3)
	      (>= (length str) 4)   ; "0:00"
	      (<= (length str) 5))  ; "99:59"
	 t)
	(t nil)))

(defun str-to-time (str)
  (make-time :minute (read-from-string
		      (subseq str 0 (position #\: str)))
	     :second (read-from-string
		      (subseq str (1+ (position #\: str)) (length str)))))

(defun destring (list)
  "takes a list of strings;  returns list of symbols;
   times remain strings in new list."
  (let ((new-list nil))
    (dolist (element list)
      (if (find #\: element)
	  (if (time-able element)
	      (push (str-to-time element) new-list)
	      (progn
		(format t "cdp.lisp: ~S: bad time~%" element)
		(quit)))
	  (push (read-from-string element) new-list)))
    (reverse new-list)))


(defun parse-args ()
  (cond ((null *args*) nil)
	((assoc (car *args*) *options*)
	 (push (cdr (assoc (pop *args*) *options*)) *flags*))
	((assoc (car *args*) *commands*)
	 (cond (*action*
		(format t "cdp.lisp: extranious commands: \"~A\"~%" (car *args*))
		(quit))
	       (t (setq *action* (cdr (assoc (pop *args*) *commands*))))))
	((numberp (car *args*))
	 (cond ((null *action*)
		(setq *action* 'play-track)
		(push (pop *args*) *command-data*))
	       ((find *action* *takes-nums*)
		(push (pop *args*) *command-data*))
	       (t
		(format t "cdp.lisp: junk on cummand line: \"~A\"~%" (car *args*))
		(quit))))
	((time-p (car *args*))
	 (cond ((null *action*)
		(setq *action* 'seek-time)
		(push (pop *args*) *command-data*))
	       ((find *action* *takes-times*)
		(push (pop *args*) *command-data*))
	       (t
		(format t "cdp.lisp: junk on command line: \"~A\"~%" (car *args*))
		(quit))))
	(t
	 (format t "cdp.lisp: junk on command line: \"~A\"~%" (car *args*))
	 (quit)))

  (if *args* (parse-args)))


;;;; done defining things.  ready to do things.

(setq *args* (destring *args*))

(parse-args)

(unless *action*
  (setq *action* 'status))

(setq *command-data* (reverse *command-data*))

(cond ((find :sbcl *features*)
       (load "cdp-actions.x86f")
       (funcall *action*)
       (unless (find 'interactive *flags*)
	 (quit)))
      (t (load "cdp-actions.lisp")))
