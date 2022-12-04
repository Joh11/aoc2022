(in-package #:aoc2022)

(defun d4/load-data ()
  (with-open-file (f "data/day4")
    (loop for l = (read-line f nil)
	  while l
	  collect (multiple-value-bind (_ groups)
		      (ppcre:scan-to-strings "(.*)-(.*),(.*)-(.*)" l)
		    (declare (ignore _))
		    (destructuring-bind (a b c d)
			(mapcar #'parse-integer (coerce groups 'list))
		      (list (interval a b) (interval c d)))))))

(defun interval (a b)
  (loop for x from a below (1+ b) collect x))

(defun d4/1 ()
  (count-if (lambda (x)
	      (destructuring-bind (a b) x
		(or (subsetp a b) (subsetp b a))))
	    (d4/load-data)))

(defun d4/2 ()
  (count-if (lambda (x)
	      (destructuring-bind (a b) x
		(intersection a b)))
	    (d4/load-data)))
