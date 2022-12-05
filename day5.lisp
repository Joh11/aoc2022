(in-package #:aoc2022)

(defun d5/load-data ()
  (with-open-file (f "data/day5")
    (loop for l = (read-line f nil)
	  with reading = :stack
	  with stack = nil
	  with instructions = nil
	  while l
	  when (eq reading :stack)
	    do (if (string= l "")
		   (setf reading :instructions)
		   (push l stack))
	  when (eq reading :instructions)
	    do (push l instructions)
	  finally (return (values
			   (make-stack (cdr stack))
			   (mapcar #'parse-instruction (cdr (nreverse instructions))))))))

(defun parse-instruction (line)
  (multiple-value-bind (_ numbers)
      (ppcre:scan-to-strings "move (.*) from (.*) to (.*)" line)
    (declare (ignore _))
    (loop for x across numbers collect (parse-integer x))))

(defun line-to-crates (line)
  (loop for x across line for n from 3 when (multiple? n 4) collect x))

(defun line-to-nstack (line)
  (/ (1+ (length line)) 4))

(defun make-stack (lines)
  (let* ((nstack (line-to-nstack (car lines)))
	 (stacks (loop for n below nstack collect nil)))
    (dolist (l lines stacks)
      (loop for x in (line-to-crates l)
	    for n from 0
	    when (not (char= x #\ ))
	      do (push x (elt stacks n))))))

(defun apply-instruction! (instruction stack)
  (destructuring-bind (n from to) instruction
    (dotimes (i n)
      (apply-instruction-one! from to stack))))

(defun apply-instruction-one! (from to stack)
  (let ((x (car (elt stack (1- from)))))
    (pop (elt stack (1- from)))
    (push x (elt stack (1- to)))))

(defun top-of-stack (stack)
  (mapcar #'car stack))

(defun apply-instruction-9001! (instruction stack)
  (destructuring-bind (n from to) instruction
    (let (xs)
      (loop for i below n
	    do (push (pop (elt stack (1- from))) xs))
      (loop for i below n
	    do (push (pop xs) (elt stack (1- to)))))))

(defun d5/1 ()
  (multiple-value-bind (stack instructions) (d5/load-data)
    (dolist (ins instructions)
      (apply-instruction! ins stack))
    (coerce (top-of-stack stack) 'string)))

(defun d5/2 ()
  (multiple-value-bind (stack instructions) (d5/load-data)
    (dolist (ins instructions)
      (apply-instruction-9001! ins stack))
    (coerce (top-of-stack stack) 'string)))
