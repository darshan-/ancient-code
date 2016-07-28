;;;; cdp-actions.lisp - defines interface to cdrom actions defined in cdrom.so


(defvar *args*)
(defvar *command-data*)


;;; i don't know why this needs to be up here instead of in the cond below,
;;;  but it doesn't work that way.
(if (find :sbcl *features*)
    (load-1-foreign "/home/josiah/lisp/cdp/cdrom.so"))

(cond ((find :sbcl *features*)
       (def-alien-routine c_init void)
       (def-alien-routine c_play_pause void)
       (def-alien-routine c_stop void)
       (def-alien-routine c_forward void)
       (def-alien-routine c_info void (track INT))
       (def-alien-routine c_status void)
       (c_init))
      (t (load "cdrom-fake.lisp")))

(defun play-pause ()
  (c_play_pause))

(defun play ()
  (format t "i might play~%"))

(defun pause ()
  (format t "i might pause~%"))

(defun stop ()
  (c_stop))

(defun forward ()
  (c_forward))

(defun fforward ()
  (format t "i might fforward~%"))

(defun backward ()
  (format t "i might backward~%"))

(defun bbackward ()
  (format t "i might bbackward~%"))

(defun seekf ()
  (format t "i might seekf~%"))

(defun seekb ()
  (format t "i might seekb~%"))

(defun tray-open ()
  (format t "i might tray-open~%"))

(defun tray-close ()
  (format t "i might tray-close~%"))

(defun info ()
  (let ((track (pop *command-data*)))
    (if (null track)
	(c_info 0)
	(let ((junk (pop *command-data*)))
	  (if (and (null junk) (numberp track))
	      (c_info track)
	      (progn
		(format t "junk on command line: \"~A\"~%" (or junk track))
		(quit)))))))

(defun play-list ()
  (format t "i might play-list~%"))

(defun play-track ()
  (format t "i might play-track~%"))

(defun seek-time ()
  (format t "i might seek-time~%"))

(defun status ()
  (c_status))
