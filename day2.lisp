(in-package #:aoc2022)

(defun d2/load-data ()
  (with-open-file (f "data/day2")
    (let (my-moves opponent-moves)
      (loop for l = (read-line f nil)
	    while l
	    do
	       (push (char-to-move (aref l 0)) opponent-moves)
	       (push (char-to-move (aref l 2)) my-moves))
      (values opponent-moves my-moves))))

(defun d2/load-data2 ()
  (with-open-file (f "data/day2")
    (let (opponent-moves rounds)
      (loop for l = (read-line f nil)
	    while l
	    do
	       (push (char-to-move (aref l 0)) opponent-moves)
	       (push (char-to-round (aref l 2)) rounds))
      (values opponent-moves rounds))))

(defun char-to-move (c)
  (ccase c
    ((#\A #\X) :rock)
    ((#\B #\Y) :paper)
    ((#\C #\Z) :scissor)))

(defun char-to-round (c)
  (ccase c ((#\X) nil) ((#\Y) :draw) ((#\Z) t)))

(defun win? (my-move opponent-move)
  (cond
    ((eq my-move opponent-move) :draw)
    ((or (and (eq my-move :rock) (eq opponent-move :scissor))
	 (and (eq my-move :paper) (eq opponent-move :rock))
	 (and (eq my-move :scissor) (eq opponent-move :paper)))
     t)
    (t nil)))

(defun shape-score (my-move)
  (ccase my-move (:rock 1) (:paper 2) (:scissor 3)))

(defun round-score (my-move opponent-move)
  (ecase (win? my-move opponent-move)
    ((nil) 0)
    (:draw 3)
    (t 6)))

(defun score (my-move opponent-move)
  (+ (shape-score my-move)
     (round-score my-move opponent-move)))

(defun guess-my-move (opponent-move round)
  (ecase round
    ((nil) (ecase opponent-move (:rock :scissor) (:paper :rock) (:scissor :paper)))
    (:draw opponent-move)
    (t (ecase opponent-move (:rock :paper) (:paper :scissor) (:scissor :rock)))))

(defun d2/1 ()
  (multiple-value-bind (opponent-moves my-moves) (d2/load-data)
    (reduce #'+ (mapcar #'score my-moves opponent-moves))))

(defun d2/2 ()
  (multiple-value-bind (opponent-moves rounds) (d2/load-data2)
    (reduce #'+ (mapcar (lambda (opponent-move round)
			  (score (guess-my-move opponent-move round) opponent-move))
			opponent-moves rounds))))
