(defconstant units #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(defun 0-to-9 (number)
	(if (and (>= number 0) (<= number 9)) 
		(return-from 0-to-9 (aref units number))
		(return-from 0-to-9 "Enter a number between 0 and 9")))

(defmacro assert-equal (num fun)
	   `(string-equal (format nil "~r" ,num) (,fun ,num)))