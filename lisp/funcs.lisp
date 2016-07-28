(defun single? (l)
  (if
      (and
       (listp l)
       (not (null l))
       (null (cdr l)))
      t
    nil))

(defun my-last (l)
  (if
      (atom l)
      l
    (if
	(single? l)
	l
      (my-last (cdr l)))))

(defun all-but-last (l)
  (if
      (single? l)
      ()
    (cons (car l) (all-but-last (cdr l)))))

(defun my-reverse (l)
  (if
      (atom l)
      nil
    (if
	(single? l)
	l
      (cons (car (my-last l)) (my-reverse (all-but-last l))))))

(defun pow (m n)
  (if
      (eq n 1)
      m
    (* m (pow m (1- n)))))