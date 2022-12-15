(in-package #:aoc2022)

(defun parse-rock-path (line)
  (mapcar (lambda (x) (mapcar #'parse-integer (ppcre:split "," x)))
	  (ppcre:split " -> " line)))

(defun d14/load-data ()
  (with-open-file (f "data/day14")
    (loop for l = (read-line f nil)
	  while l
	  collect (parse-rock-path l))))

(defun add-path-one-line (map a b)
  (destructuring-bind ((xa ya) (xb yb)) (list a b)
    (cond ((= xa xb) (loop for y from (min ya yb) to (max ya yb) do (setf (aref map xa y) t)))
	  ((= ya yb) (loop for x from (min xa xb) to (max xa xb) do (setf (aref map x ya) t)))
	  (t (error "should not happen")))))

(defun add-rock-path (map path)
  (loop for xs on path
	while (>= (length xs) 2)
	as a = (first xs)
	as b = (second xs) do
	  (add-path-one-line map a b)))

(defun find-rock-bounds (paths)
  (let ((all-x (mapcar #'first (apply #'append paths)))
	(all-y (mapcar #'second (apply #'append paths))))
    (list (reduce #'min all-x)
	  (reduce #'max all-x)
	  (reduce #'min all-y)
	  (reduce #'max all-y))))

(defun translate-path (shiftx path)
  (mapcar
   (lambda (pair) (destructuring-bind (x y) pair (list (- x shiftx) y)))
   path))

(defun tile-free? (map x y)
  (and (< -1 x (array-dimension map 0))
       (< -1 y (array-dimension map 1))
       (not (aref map x y))))

(defun update-sand (map sand-position)
  (destructuring-bind (x y) sand-position
    (cond ((tile-free? map x (1+ y)) (list x (1+ y)))
	  ((tile-free? map (1- x) (1+ y)) (list (1- x) (1+ y)))
	  ((tile-free? map (1+ x) (1+ y)) (list (1+ x) (1+ y)))
	  (t sand-position))))

(defun update-sand-until-rest (map sand-position ycutoff &optional cutoff-is-ground?)
  (loop while t
	as new-position = (update-sand map sand-position) do
	  (when (> (second new-position) ycutoff) (return (if cutoff-is-ground? sand-position nil)))
	  (when (equal sand-position new-position) (return new-position))
	  (setf sand-position new-position)))

(defun make-rocks (paths &optional (margin 500))
  ;; some margin to simulate a box around the rocks
  (destructuring-bind (minx maxx miny maxy) (find-rock-bounds paths)
    (declare (ignore miny))
    (let* ((shiftx (- minx margin))
	   (lengthx (- (+ maxx margin) shiftx))
	   (map (make-array (list lengthx (+ maxy margin))
			    :initial-element nil)))
      ;; shift the paths
      (print paths)
      (setf paths (mapcar (partial translate-path shiftx) paths))
      (print paths)
      ;; fill the map
      (dolist (path paths) (add-rock-path map path))
      (list shiftx maxy map))))

(defun d14/1 ()
  (destructuring-bind (shiftx maxy map) (make-rocks (d14/load-data))
    (let ((initial-sand-position (list (- 500 shiftx) 0)))
      (loop as sand-position = (update-sand-until-rest map initial-sand-position maxy)
	    for n from 0
	    while sand-position do
	      (destructuring-bind (x y) sand-position
		(setf (aref map x y) :sand))
	    finally (return n)))))

(defun d14/2 ()
  (destructuring-bind (shiftx maxy map) (make-rocks (d14/load-data))
    (let ((initial-sand-position (list (- 500 shiftx) 0)))
      (loop as sand-position = (update-sand-until-rest map initial-sand-position (+ 1 maxy) t)
	    for n from 0
	    while (tile-free? map (first initial-sand-position) (second initial-sand-position)) do
	      (destructuring-bind (x y) sand-position
		(setf (aref map x y) :sand))
	    finally (return n)))))

(defun print-map (map)
  (loop for y below (array-dimension map 1) do
    (loop for x below (array-dimension map 0) do
      (format t (case (aref map x y) (:sand "o") ((t) "#") ((nil) "."))))
    (format t "~%")))
