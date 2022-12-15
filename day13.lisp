(in-package #:aoc2022)

(defun d13/load-data ()
  (with-open-file (f "data/day13") ;"data/day13"
    (loop for l = (read-line f nil)
	  while l
	  unless (string= l "")
	    collect (parse-packet l) into packets
	  finally (return (group-by 2 packets)))))

(defun parse-packet (line)
  (setf line (ppcre:regex-replace-all "," line " "))
  (setf line (ppcre:regex-replace-all "\\[" line "("))
  (setf line (ppcre:regex-replace-all "\\]" line ")"))
  (read-from-string line))

(defun packet-compare (a b)
  ;; (print (list a b))
  (cond ((and (numberp a) (numberp b)) (if (= a b) :unknown (< a b)))
	((and (listp a) (listp b))
	 (loop for x in a for y in b do
	   (case (packet-compare x y)
	     ((t) (return t))
	     ((nil) (return nil)))
	       finally (return (packet-compare (length a) (length b)))))
	(t (if (numberp a) (packet-compare (list a) b) (packet-compare a (list b))))))

(defun d13/1 ()
  (loop for (a b) in (d13/load-data)
	for n from 1
	when (packet-compare a b) collect n))
