(in-package #:aoc2022)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun d7/load-data ()
  (with-open-file (f "data/day7") ;;"test"
    ;; two states: :default and :append
    ;; :default looks for commands
    ;; :append add lines to the current ls command, until a command is seen
    (let (commands (state :default))
      (labels ((process-command (l)
		 (if (string= (command-name l) "cd")
		     (push (list :cd (command-argument l)) commands)
		     ;; else it's ls
		     (progn
		       (setf state :append)
		       (push (list :ls) commands))))
	       (finish-append ()
		 (setf state :default)
		 (push (nreverse (pop commands)) commands)))
	(loop for l = (read-line f nil)
	      while l
	      do
		 (case state
		   (:default (if (command? l) (process-command l) (error "this should not happen")))
		   (:append (if (command? l)
				(progn
				  (finish-append)
				  (process-command l))
				(push (parse-ls-output l) (car commands)))))
	      finally (progn
			(when (eq state :append) (finish-append))
			(return (nreverse commands))))))))

(defun parse-ls-output (line)
  (let ((tokens (ppcre:split " " line)))
    (if (string= (first tokens) "dir")
	(list :dir (second tokens))
	(list :file (parse-integer (first tokens)) (second tokens)))))

(defun command? (line)
  (char= #\$ (aref line 0)))

(defun command-name (line)
  (second (ppcre:split " " line)))

(defun command-argument (line)
  (third (ppcre:split " " line)))

(defun mkdir-if-needed (name root)
  (aif (find-directory name root)
       it
       (let ((new-dir (new-directory name)))
	 (add-child root new-dir)
	 new-dir)))

(defun cd-command (cmd directory-stack-wrap)
  (let ((dir (second cmd)))
    (cond ((string= "/" dir) (setf (car directory-stack-wrap) (last (car directory-stack-wrap))))
	  ((string= ".." dir) (pop (car directory-stack-wrap)))
	  (t ;; first check if needed to create a new directory
	   (let ((new-dir (mkdir-if-needed dir (caar directory-stack-wrap))))
	     (push new-dir (car directory-stack-wrap)))))))

(defun find-directory (name root)
  (loop for child in (cddr root)
	when (and (eq (car child) :dir) (string= (second child) name))
	  do (return child)
	finally (return)))

(defun new-directory (name)
  (list :dir name))

(defun add-child (root child)
  (setf (cddr root) (cons child (cddr root))))

(defun ls-command (cmd root)
  (dolist (entry (cdr cmd))
    (ecase (car entry)
      (:file (add-child root entry))
      (:dir (mkdir-if-needed (second entry) root)))))

(defun build-tree (commands)
  (loop for cmd in commands
	with root = (new-directory "/")
	with directory-stack-wrap = (list (list root))
	do
	   (ecase (car cmd)
	     (:cd (cd-command cmd directory-stack-wrap))
	     (:ls (ls-command cmd (caar directory-stack-wrap))))
	finally (return root)))

(defun do-dirs (function root)
  (when (eq (car root) :dir) ;; if it is a directory, not a file
    (funcall function root)
    (loop for child in (cddr root)
	  do (do-dirs function child))))

(defun do-files (function root)
  (case (car root)
    (:dir (loop for child in (cddr root)
		do (do-files function child)))
    (:file (funcall function root))))

(defun file-size (file) (second file))

(defun compute-dir-size (root)
  (let ((size 0))
    (do-files
	(lambda (file) (incf size (file-size file)))
      root)
    size))

(defun all-directory-sizes (root)
  (let (dir-sizes)
    (do-dirs (lambda (dir) (push (compute-dir-size dir) dir-sizes))
      root)
    dir-sizes))

(defun d7/1 ()
  (let* ((root (build-tree (d7/load-data)))
	 (dir-sizes (all-directory-sizes root)))
    (sum (remove-if-not (partial-end <= 100000) dir-sizes))))

(defun d7/2 ()
  (let* ((root (build-tree (d7/load-data)))
	 (dir-sizes (all-directory-sizes root))
	 (overload (- 30000000 (- 70000000 (compute-dir-size root)))))
    (car (sort (remove-if (partial-end < overload) dir-sizes) #'<=))))
