(in-package #:aoc2022)

(defun d10/load-data ()
  (with-open-file (f "data/day10")
    (loop for l = (read-line f nil)
	  while l
	  collect (parse-cpu-instruction l))))

(defun parse-cpu-instruction (line)
  (let ((tokens (ppcre:split " " line)))
    (if (string= (car tokens) "noop")
	(list :noop)
	(list :addx (parse-integer (second tokens))))))

(defun cpu-instruction-cycle (instruction)
  (case (car instruction)
    (:noop 1) (:addx 2)))

(defun run-cpu-instructions (instructions)
  (loop for n from 1
	as instruction = (car instructions)
	while (not (null instructions))
	with x = 1
	with xs = nil
	;; or ((timer . instruction)...)
	with current = nil do
	  ;; maybe schedule future instruction
	  (unless current
	    (setf instructions (cdr instructions))
	    (setf current (cons (cpu-instruction-cycle instruction) instruction)))
	  ;; record x
	  (push x xs)
	  ;; run scheduled instruction
	  (when current
	    (decf (car current))
	    (when (= 0 (car current))
	      (case (second current)
		(:noop)
		(:addx (incf x (third current))))
	      (setf current nil)))
	finally (return (nreverse xs))))

(defun empty-crt ()
  (make-array '(6 40) :element-type 'boolean :initial-element nil))

(defun draw-crt (crt)
  (dotimes (i 6)
    (dotimes (j 40) (format t "~a" (if (aref crt i j) "#" ".")))
    (format t "~%")))

(defun crt-from-register (register)
  (loop for sprite-position in register
	for n from 0
	as col = (mod n 40)
	with crt = (empty-crt) do
	  (when (<= (abs (- sprite-position col)) 1)
	    (setf (row-major-aref crt n) t))
	finally (return crt)))

(defun d10/1 ()
  (loop for n in '(20 60 100 140 180 220)
	with signals = (run-cpu-instructions (d10/load-data))
	sum (* n (elt signals (1- n)))))

(defun d10/2 ()
  (draw-crt (crt-from-register (run-cpu-instructions (d10/load-data)))))
