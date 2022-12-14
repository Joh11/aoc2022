(in-package #:aoc2022)

(defun d12/load-data ()
  (with-open-file (f "data/day12")
    (loop for l = (read-line f nil)
	  with start = nil
	  with end = nil
	  for row from 0
	  as col = 0
	  while l
	  collect (map 'list (lambda (c)
			       (prog1 (cond ((char= c #\S) (setf start (list row col)) 0)
					    ((char= c #\E) (setf end (list row col)) 27)
					    (t (- (char-code c) (char-code #\a) -1)))
				 (incf col)))
		       l)
	    into rows
	  finally (return (list start end (make-array (list (length rows) (length (car rows)))
						      :initial-contents rows))))))

(defun valid-coords? (array &rest coords)
  (and (= (length coords) (array-rank array))
       (every (lambda (i n) (and (< i n) (<= 0 i))) coords (array-dimensions array))))

(defun can-climb? (current-height next-height)
  (<= (- next-height current-height) 1))

(defun available-moves (hm starting-point pred)
  (destructuring-bind (i j) starting-point
    (let ((x (aref hm i j)) moves)
      (let ((k (1+ i)) (l j)) (when (and (valid-coords? hm k l) (funcall pred x (aref hm k l))) (push (list k l) moves)))
      (let ((k (1- i)) (l j)) (when (and (valid-coords? hm k l) (funcall pred x (aref hm k l))) (push (list k l) moves)))
      (let ((k i) (l (1+ j))) (when (and (valid-coords? hm k l) (funcall pred x (aref hm k l))) (push (list k l) moves)))
      (let ((k i) (l (1- j))) (when (and (valid-coords? hm k l) (funcall pred x (aref hm k l))) (push (list k l) moves)))
      moves)))

(defun shortest-path-tree (hm starting-point pred)
  (let ((distances (make-array (array-dimensions hm) :initial-element (1+ (array-total-size hm)))))
    (setf (aref distances (first starting-point) (second starting-point)) 1)
    (labels ((rec (i j step)
	       (unless (<= (aref distances i j) step) ;; skip if we already have a fast enough path
		 (setf (aref distances i j) step)
		 (let ((moves (available-moves hm (list i j) pred)))
		   (dolist (move moves)
		     (rec (first move) (second move) (1+ step)))))))
      (rec (first starting-point) (second starting-point) 0)
      distances)))

(defun find-indices-2d (arr pred)
  (loop for i below (array-dimension arr 0)
	with indices = nil do
	  (loop for j below (array-dimension arr 1)
		when (funcall pred (aref arr i j)) do
		  (push (list i j) indices))
	finally (return (nreverse indices))))

(defun d12/1 ()
  (destructuring-bind (starting-point end-point hm) (d12/load-data)
    (aref (shortest-path-tree hm starting-point #'can-climb?) (first end-point) (second end-point))))

(defun d12/2 ()
  (destructuring-bind (starting-point end-point hm) (d12/load-data)
    (declare (ignore starting-point))
    (loop for (i j) in (find-indices-2d hm (partial-end <= 1))
	  with sp = (shortest-path-tree hm end-point (swap-args can-climb?))
	  collect (aref sp i j) into steps
	  finally (return (apply #'min steps)))))
