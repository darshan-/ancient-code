;;;; wolfies.lisp
;;;; solves wolf problem

;; 0x - white
;; 1x - black
;; 2x - standing
;; 3x - jumping

;; x0 - head
;; x1 - tail

(defvar *cards* '((11 20 00 30)
				(20 00 10 31)
				(11 20 01 21)
				(20 31 10 01)
				(11 01 21 30)
				(31 30 10 21)
				(01 00 11 31)
				(11 30 21 01)
				(11 01 21 31))
  "The card set that binds the problem.")

(defvar *deck* *cards*
  "the cards as we hold them in our hand.")

(defvar *board* '(nil nil nil
				  nil nil nil
				  nil nil nil)
  "the board we are considering.")

(defun wolf-half (card-pos wolf-pos)
  "returns wolf half at WOLF-POS (position of wolf on card) of CARD-POS (position of card on *board*)."
  (elt (elt *board* card-pos) wolf-pos))

(defun rotate-card (card)
  (append (cdr card) (list (car card))))

(defun same-cardp (card1 card2)
  (let* ((card1-a card1)
		 (card1-b (rotate-card card1-a))
		 (card1-c (rotate-card card1-b))
		 (card1-d (rotate-card card1-c)))
	(or (equal card2 card1-a)
		(equal card2 card1-b)
		(equal card2 card1-c)
		(equal card2 card1-d))))

(defun fitp-help (wh1 wh2)
  "true if wolf half WH1 fits wolf half WH2."
  (eql 1 (abs (- wh1 wh2))))

(defun fitp (card pos)
  "returns true if CARD fits at POS of *board*."
  (case pos
	(0 t)
	((1 2) (fitp-help (elt card 3) (wolf-half (- pos 1) 1)))
	((3 6) (fitp-help (elt card 0) (wolf-half (- pos 3) 2)))
	((4 5 7 8) (and (fitp-help (elt card 3) (wolf-half (- pos 1) 1))
					(fitp-help (elt card 0) (wolf-half (- pos 3) 2))))))

(defun find-fit (pos deck)  ;; note: if card has two of matching wolf half, only catches first
  "returns a list of cards that fit into next POS."
  (let* ((card-a (car deck))
		 (card-b (rotate-card card-a))
		 (card-c (rotate-card card-b))
		 (card-d (rotate-card card-c)))
	(cond ((null deck)
		   nil)
		  ((fitp card-a pos)
		   (cons card-a (find-fit pos (cdr deck))))
		  ((fitp card-b pos)
		   (cons card-b (find-fit pos (cdr deck))))
		  ((fitp card-c pos)
		   (cons card-c (find-fit pos (cdr deck))))
		  ((fitp card-d pos)
		   (cons card-d (find-fit pos (cdr deck))))
		  (t
		   (find-fit pos (cdr deck))))))

(defun find-rest (pos)
  "searches "
  (when (> pos 8)
	(format t "a solution: ~A~%" *board*))
  (dolist (card (find-fit pos *deck*))
	(setf (elt *board* pos) card)
	(setf *deck* (remove card *deck* :test #'same-cardp :count 1))
	(find-rest (1+ pos))
	(setf *deck* (append *deck* (list card)))))

(defun find-solution ()
  "finds any solutions."
  (dolist (card *cards*)
	(setf (car *board*) card)
	(setf *deck* (remove card *deck* :test #'same-cardp :count 1))
	(find-rest 1)
	(setf (car *board*) (rotate-card (car *board*)))
	(find-rest 1)
	(setf (car *board*) (rotate-card (car *board*)))
	(find-rest 1)
	(setf (car *board*) (rotate-card (car *board*)))
	(find-rest 1)
	(setf *deck* (append *deck* (list card)))))

(defun reset-game ()
  (setf *board* '(nil nil nil nil nil nil nil nil nil))
  (setf *deck* *cards*))
