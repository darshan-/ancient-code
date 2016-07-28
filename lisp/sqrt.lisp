(defun jsqrt (x)
  (jsqrt-iter 1.0 x))

(defun jsqrt-iter (guess x)
  (let ((guess2 (improve guess x)))
    (if (< (abs (- guess2 guess)) (/ x 1.0E12))
	guess
	(jsqrt-iter guess2 x))))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))
