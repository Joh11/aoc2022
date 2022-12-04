;;;; aoc2022.asd

(asdf:defsystem #:aoc2022
  :description "Describe aoc2022 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "common")
	       (:file "day1")))
