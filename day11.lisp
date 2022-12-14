(in-package #:aoc2022)

(defun parse-starting-items (line)
  (mapcar #'parse-integer (ppcre:split ", " (subseq line 18))))

(defun parse-monkey-operation (line)
  (destructuring-bind (a op b) (ppcre:split " " (subseq line 19))
    (setf a (if (string= a "old") 'old (parse-integer a)))
    (setf b (if (string= b "old") 'old (parse-integer b)))
    (eval `(lambda (old) (,(intern op) ,a ,b)))))

(defun parse-monkey-test (line1 line2 line3)
  (mapcar (lambda (line) (parse-integer (car (last (ppcre:split " " line)))))
	  (list line1 line2 line3)))

(defun parse-monkey (f)
  (let* ((items (parse-starting-items (read-line f)))
	 (operation (parse-monkey-operation (read-line f)))
	 (test (parse-monkey-test (read-line f) (read-line f) (read-line f))))
    (read-line f nil)
    (list items operation test)))

(defun d11/load-data ()
  (with-open-file (f "data/day11")
    (loop for l = (read-line f nil)
	  while l
	  collect (parse-monkey f) into monkeys
	  finally (return (make-array (length monkeys) :initial-contents monkeys)))))

(defun next-monkey (test level)
  (destructuring-bind (div a b) test
    (if (multiple? level div) a b)))

(defun all-monkeys-round (monkeys)
  (loop for (items operation test) across monkeys
	with ninspected-items = nil
	for n from 0 do
	  (push 0 ninspected-items)
	  (loop for item in items for i from 0 do
	    (incf (car ninspected-items))
	    (let* ((new-level (floor (funcall operation item) 3))
		   (next-monkey (next-monkey test new-level)))
	      ;; append at the back of the items of the monkey
	      (setf (car (aref monkeys next-monkey))
		    (nconc (car (aref monkeys next-monkey)) (list new-level)))))
	  (setf (car (aref monkeys n)) nil)
	finally (return (nreverse ninspected-items))))

(defun d11/1 ()
  (let* ((monkeys (d11/load-data))
	 (inspected-items (loop for i below (array-dimension monkeys 0) collect 0)))
    (dotimes (i 20)
      (setf inspected-items (mapcar #'+ inspected-items (all-monkeys-round monkeys))))
    (setf inspected-items (sort inspected-items #'>))
    (* (first inspected-items) (second inspected-items))))

