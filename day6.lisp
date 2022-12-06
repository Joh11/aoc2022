(in-package #:aoc2022)

(defun d6/load-data ()
  (with-open-file (f "data/day6")
    (coerce (read-line f) 'list)))

(defun all-different (xs &key (eq-fn #'eq))
  (if (null xs) t
      (and (notany (lambda (x) (funcall eq-fn x (car xs))) (cdr xs))
	   (all-different (cdr xs) :eq-fn eq-fn))))

(defun d6/1 ()
  (loop for xs on (d6/load-data)
	for n from 0
	when (all-different (get-n-first 4 xs) :eq-fn #'char=)
	  do (return (+ 4 n))))

(defun d6/2 ()
  (loop for xs on (d6/load-data)
	for n from 0
	when (all-different (get-n-first 14 xs) :eq-fn #'char=)
	  do (return (+ 14 n))))
