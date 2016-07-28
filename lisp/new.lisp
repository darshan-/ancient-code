(defun numline (n)
  (list
   (if (zerop n)
       t
       nil)
   (if (< n 0)
       t
       nil)))
