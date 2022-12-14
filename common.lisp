(in-package #:aoc2022)

(defun sum (list)
  (reduce #'+ list))

(defun multiple? (a b)
  (= 0 (mod a b)))

(defun get-n-first (n xs)
  (loop for x in xs
	for i below n
	collect x))

(defun group-by (n xs)
  (assert (multiple? (length xs) n))
  (loop for x on xs
	for i from 0
	when (multiple? i n)
	  collect (get-n-first n x)))

(defmacro partial-end (fun &rest args)
  (let ((x (gensym)))
    `(lambda (,x) (,fun ,x ,@args))))

(defmacro partial (fun &rest args)
  (let ((x (gensym)))
    `(lambda (,x) (,fun ,@args ,x))))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro swap-args (fun)
  `(lambda (a b) (,fun b a)))
