
(load-1-foreign "/home/josiah/lisp/cdp/cdrom.so")

(defun play-pause ()
  (format t "i might play-pause~%"))

(defun play ()
  (format t "i might play~%"))

(defun pause ()
  (format t "i might pause~%"))

(defun stop ()
  (format t "i might stop~%"))

(defun forward ()
  (format t "i might forward~%"))

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
  (format t "i might info~%"))

(defun play-list ()
  (format t "i might play-list~%"))

(defun play-track ()
  (format t "i might play-track~%"))

(defun seek-time ()
  (format t "i might seek-time~%"))

(defun status ()
  (def-alien-routine c_status void)
  (c_status))
