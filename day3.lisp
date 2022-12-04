(in-package #:aoc2022)

(defun d3/load-data ()
  (with-open-file (f "data/day3")
    (loop for l = (read-line f nil)
	  while l
	  collect l)))

(defun shared-type (rucksack)
  (let ((n (length rucksack)))
    (assert (multiple? n 2)) ;; even number of elements
    (let ((xs (coerce (subseq rucksack 0 (/ n 2)) 'list))
	  (ys (coerce (subseq rucksack (/ n 2)) 'list)))
      (loop for x in xs
	       when (member x ys)
		 do (return x)))))

(defun type-priority (type)
  (let ((n (char-code type)))
    (assert (and (>= n (char-code #\A))
		 (<= n (char-code #\z))))
    (if (< n (char-code #\a))
	(+ 27 n (- (char-code #\A)))
	(+ 1 n (- (char-code #\a))))))

(defun find-common-type (rucksacks)
  (destructuring-bind (xs ys zs) rucksacks
    (car (loop for x in xs
	       when (and (member x ys) (member x zs))
		 collect x))))

(defun d3/1 ()
  (sum
   (mapcar (lambda (rucksack)
	     (type-priority (shared-type rucksack)))
	   (d3/load-data))))

(defun d3/2 ()
  (sum (mapcar (lambda (rucksacks)
		 (type-priority (find-common-type rucksacks)))
	       (group-by 3 (mapcar (partial-end coerce 'list) (d3/load-data))))))
