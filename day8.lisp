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

(defun visibility-map (height-map)
  (let ((vmap (make-array (array-dimensions height-map)
			  :element-type 'boolean
			  :initial-element nil))
	(h (array-dimension height-map 0))
	(w (array-dimension height-map 1)))
    ;; first fill the contour
    (loop for i below h do
      (setf (aref vmap i 0) t)
      (setf (aref vmap i (1- h)) t))
    (loop for j below w do
      (setf (aref vmap 0 j) t)
      (setf (aref vmap (1- w) j) t))

    ;; now the interior
    (dotimes (j w)
      ;; top to bottom
      (loop for i from 1 to (- h 2)
	    with min = (aref height-map 0 j)
	    as current = (aref height-map i j)
	    do
	       (when (> current min) (setf (aref vmap i j) t))
	       (setf min (max min current)))
      ;; bottom to top
      (loop for i from (- h 2) downto 1
	    with min = (aref height-map (1- h) j)
	    as current = (aref height-map i j)
	    do
	       (when (> current min) (setf (aref vmap i j) t))
	       (setf min (max min current))))
    (dotimes (i h)
      ;; left to right
      (loop for j from 1 to (- w 2)
	    with min = (aref height-map i 0)
	    as current = (aref height-map i j)
	    do
	       (when (> current min) (setf (aref vmap i j) t))
	       (setf min (max min current)))
      ;; right to left
      (loop for j from (- w 2) downto 1
	    with min = (aref height-map i (1- w))
	    as current = (aref height-map i j)
	    do
	       (format t "min ~a current ~a (~a ~a)~%" min current i j)
	       (when (> current min) (setf (aref vmap i j) t))
	       (setf min (max min current))))
    vmap))

(defun print-height-map (hmap &optional (format-fn #'identity))
  (destructuring-bind (h w) (array-dimensions hmap)
    (dotimes (i h)
      (dotimes (j w)
	(format t "~a " (funcall format-fn (aref hmap i j))))
      (format t "~%"))))

(defun d8/1 ()
  (let ((vmap (visibility-map (d8/load-data))))
    (loop for n below (array-total-size vmap)
	  count (row-major-aref vmap n))))
