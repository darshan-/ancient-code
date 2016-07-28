;;;; cdrom-fake.lisp - fake c functions for testing with lisps other than sbcl


(defun c_play_pause ()
  (format t "fake c_play_pause~%"))

(defun c_play ()
  (format t "fake c_play~%"))

(defun c_pause ()
  (format t "fake c_pause~%"))

(defun c_stop ()
  (format t "fake c_stop~%"))

(defun c_forward ()
  (format t "fake c_forward~%"))

(defun c_fforward ()
  (format t "fake c_fforward~%"))

(defun c_backward ()
  (format t "fake c_backward~%"))

(defun c_bbackward ()
  (format t "fake c_bbackward~%"))

(defun c_seekf ()
  (format t "fake c_seekf~%"))

(defun c_seekb ()
  (format t "fake c_seekb~%"))

(defun c_tray_open ()
  (format t "fake c_tray_open~%"))

(defun c_tray_close ()
  (format t "fake c_tray_close~%"))

(defun c_info (track)
  (if (zerop track)
      (format t "fake info on all tracks~%")
      (format t "fake info on track ~A~%" track)))

(defun c_play_list ()
  (format t "fake c_play_list~%"))

(defun c_play_track ()
  (format t "fake c_play_track~%"))

(defun c_seek_time ()
  (format t "fake c_seek_time~%"))

(defun c_status ()
  (format t "fake c_status~%"))
