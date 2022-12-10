(asdf:defsystem #:aoc2022
  :description "Describe aoc2022 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "common")
	       (:file "day1")
	       (:file "day2")
	       (:file "day3")
	       (:file "day4")
	       (:file "day5")
	       (:file "day6")
	       (:file "day7")
	       (:file "day8")
	       (:file "day9")
	       (:file "day10")))
