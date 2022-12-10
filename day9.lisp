(in-package #:aoc2022)

(defun d9/load-data ()
  (with-open-file (f "data/day9")
    (loop for l = (read-line f nil)
	  while l
	  collect (parse-rope-motion l))))

(defun parse-rope-motion (line)
  (destructuring-bind (dir n) (ppcre:split " " line)
    (assert (member dir '("U" "D" "L" "R") :test 'equal))
    (list (intern dir :keyword) (parse-integer n))))

(defun move-head (head direction)
  (destructuring-bind (x y) head
    (ecase direction
      (:u (list x (1+ y)))
      (:d (list x (1- y)))
      (:l (list (1- x) y))
      (:r (list (1+ x) y)))))

(defun head-tail-touching? (head tail)
  (destructuring-bind (hx hy) head
    (destructuring-bind (tx ty) tail
      (and (<= (abs (- hx tx)) 1)
	   (<= (abs (- hy ty)) 1)))))

(defun head-tail-same-row-col? (head tail)
  (destructuring-bind (hx hy) head
    (destructuring-bind (tx ty) tail
      (or (= hx tx) (= hy ty)))))

(defun move-tail (head tail)
  (cond ((head-tail-touching? head tail) tail)
	((head-tail-same-row-col? head tail) (move-tail-same-row-col head tail))
	(t (move-tail-other head tail))))

(defun move-tail-same-row-col (head tail)
  (destructuring-bind (hx hy) head
    (destructuring-bind (tx ty) tail
      (cond ((and (= hx tx) (> hy ty)) (list tx (1- hy)))
	    ((and (= hx tx) (< hy ty)) (list tx (1+ hy)))
	    ((and (> hx tx) (= hy ty)) (list (1- hx) ty))
	    ((and (< hx tx) (= hy ty)) (list (1+ hx) ty))
	    (t (error "something really wrong"))))))

(defun move-tail-other (head tail)
  (labels ((move-diagonal (head tail)
	     (destructuring-bind (hx hy) head
	       (destructuring-bind (tx ty) tail
		 (list (+ tx (if (> tx hx) -1 1))
		       (+ ty (if (> ty hy) -1 1)))))))
    (loop while (not (or (head-tail-same-row-col? head tail)
			 (head-tail-touching? head tail)))
	  do (setf tail (move-diagonal head tail)))
    (if (head-tail-touching? head tail)
	tail
	(move-tail-same-row-col head tail))))

(defun d9/1 ()
  (let ((head '(0 0))
	(tail '(0 0))
	(visited '((0 0))))
    (dolist (motion (d9/load-data))
      (loop for n below (second motion)
	    with direction = (car motion) do
	      (setf head (move-head head direction))
	      (setf tail (move-tail head tail))
	      (when (not (member tail visited :test 'equal))
		(push tail visited))))
    (length visited)))

(defun display-rope (rope)
  (loop for n from -10 to 10 do
    (format t "~%")
    (loop for m from -10 to 10
	  as c = (length (member (list m n) rope :test 'equal)) do
	    (if (> c 0) (format t "~a" (- 10 c)) (format t "-")))))

(defun d9/2 ()
  (let ((rope (loop for n below 10 collect (list 0 0)))
	(visited '((0 0))))
    (dolist (motion (d9/load-data))
      (dotimes (n (second motion))
	(setf (car rope) (move-head (car rope) (car motion)))
	(loop for n from 1 below (length rope)
	      for partial-rope on rope
	      as head = (car partial-rope)
	      as tail = (second partial-rope) do
		(setf (elt rope n) (move-tail head tail)))
	(let ((tail (car (last rope))))
	  (when (not (member tail visited :test 'equal))
	    (push tail visited)))))
    (length visited)))
