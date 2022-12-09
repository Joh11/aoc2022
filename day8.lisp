(in-package #:aoc2022)

(defun d8/load-data ()
  (with-open-file (f "data/day8")
    (loop for l = (read-line f nil)
	  while l
	  collect (parse-tree-line l) into lines
	  finally (return (make-array (list (length lines) (length (car lines)))
				      :initial-contents lines)))))

(defun parse-tree-line (line)
  (mapcar (lambda (c) (parse-integer (string c))) (coerce line 'list)))

(defun line-of-sight-left (array i j &optional from-edge)
  (if from-edge
	(loop for k to j collect (aref array i k))
	(nreverse (line-of-sight-left array i j t))))

(defun line-of-sight-right (array i j &optional from-edge)
  (let ((w (array-dimension array 1)))
    (if from-edge
	(nreverse (line-of-sight-right array i j))
	(loop for k from j below w collect (aref array i k)))))

(defun line-of-sight-top (array i j &optional from-edge)
  (if from-edge
      (loop for k from 0 to i collect (aref array k j))
      (nreverse (line-of-sight-top array i j t))))

(defun line-of-sight-bottom (array i j &optional from-edge)
  (let ((h (array-dimension array 0)))
    (if from-edge
	(nreverse (line-of-sight-bottom array i j))
	(loop for k from i below h collect (aref array k j)))))

(defun tree-visible? (sight)
  (> (car sight) (reduce #'max (cdr sight) :initial-value -1)))

(defun viewing-distance (sight)
  (case (length sight)
    ((0 1) 0)
    (otherwise (loop for cur in (cdr sight)
		     for n from 1
		     with max = (car sight)
		     do (when (>= cur max) (return n))
		     finally (return n)))))

(defmacro for-2d-array ((arr i j) array &body body)
  (let ((h (gensym)) (w (gensym)))
    `(let* ((,arr ,array)
	    (,h (array-dimension ,arr 0))
	    (,w (array-dimension ,arr 1)))
       (loop for ,i below ,h
	     do (loop for ,j below ,w
		      do ,@body)))))

(defun d8/1 ()
  (let ((visible-count 0))
    (for-2d-array (arr i j) (d8/load-data)
      (when (or (tree-visible? (line-of-sight-left arr i j))
		(tree-visible? (line-of-sight-right arr i j))
		(tree-visible? (line-of-sight-top arr i j))
		(tree-visible? (line-of-sight-bottom arr i j)))
	(incf visible-count)))
    visible-count))

(defun d8/2 ()
  (let (scenic-score)
    (for-2d-array (arr i j) (d8/load-data)
      (push (* (viewing-distance (line-of-sight-left arr i j))
	       (viewing-distance (line-of-sight-right arr i j))
	       (viewing-distance (line-of-sight-top arr i j))
	       (viewing-distance (line-of-sight-bottom arr i j)))
	    scenic-score))
    (reduce #'max scenic-score)))

