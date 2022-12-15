(in-package #:aoc2022)

(defun d13/load-data ()
  (with-open-file (f "data/day13")
    (loop for l = (read-line f nil)
	  while l
	  unless (string= l "")
	    collect (parse-packet l))))

(defun parse-packet (line)
  (setf line (ppcre:regex-replace-all "," line " "))
  (setf line (ppcre:regex-replace-all "\\[" line "("))
  (setf line (ppcre:regex-replace-all "\\]" line ")"))
  (read-from-string line))

(defun packet-compare (a b)
  (cond ((and (numberp a) (numberp b)) (if (= a b) :unknown (< a b)))
	((and (listp a) (listp b))
	 (loop for x in a for y in b do
	   (case (packet-compare x y)
	     ((t) (return t))
	     ((nil) (return nil)))
	       finally (return (packet-compare (length a) (length b)))))
	(t (if (numberp a) (packet-compare (list a) b) (packet-compare a (list b))))))

(defun d13/1 ()
  (loop for (a b) in (group-by 2 (d13/load-data))
	for n from 1
	when (packet-compare a b) sum n))

(defun d13/2 ()
  (let* ((divider1 '((2)))
	 (divider2 '((6)))
	 (ordered-packets (sort (append (list divider1 divider2) (d13/load-data))
				#'packet-compare)))
    (* (1+ (position divider1 ordered-packets :test #'equal))
       (1+ (position divider2 ordered-packets :test #'equal)))))
