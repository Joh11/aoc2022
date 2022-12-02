(in-package #:aoc2022)

(defun d1/load-data ()
  (with-open-file (f "data/day1")
    (let ((elves (list nil)))
      (loop for l = (read-line f nil)
	    while l
	    do (if (string= l "")
		   (push nil elves)
		   (push (parse-integer l) (car elves))))
      (nreverse (mapcar #'nreverse elves)))))

(defun sum (list)
  (reduce #'+ list))

(defun index-of-max (list)
  (if (= 0 (length list)) nil
      (let ((max (car list))
	    (idx 0))
	(loop for x in (cdr list)
	      for n from 1
	      do
		 (when (> x max)
		   (setf max x)
		   (setf idx n)))
	(values idx max))))

(defun d1/1 ()
  (let* ((data (d1/load-data))
	 (totals (mapcar #'sum data))
	 (idx (index-of-max totals)))
    (elt totals idx)))

(defun d1/2 ()
  (let* ((data (d1/load-data))
	 (totals (sort (mapcar #'sum data) #'>)))
    (+ (first totals) (second totals) (third totals))))
