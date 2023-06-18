(defconstant units #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(defconstant teens #("ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
 "eighteen" "nineteen"))
(defconstant dozen #("twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

(defmacro assert-equal (num fun)
	   `(string-equal (format nil "~r" ,num) (,fun ,num)))
	   
(defun 0-to-9 (number)
	(if (and (>= number 0) (<= number 9)) 
		(return-from 0-to-9 (aref units number))
		(return-from 0-to-9 "Enter a number between 0 and 9")))


(defun 10-to-19 (number)
	   (if (and (>= number 10) (<= number 19))
		    (return-from 10-to-19 (aref teens (- number 10)))
		    (return-from 10-to-19 "Enter a number between 10 and 19")))	   
			
(defun 20-to-99 (number)
	   (if (and (>= number 20) (<= number 99))
	       (if (> (mod number 10) 0)	       
				(return-from 20-to-99 
					(concatenate 'string 
						(aref dozen (- (floor (/ number 10)) 2))
						"-"  
						(0-to-9 (mod number 10))))
				(return-from 20-to-99 					
						(aref dozen (- (floor (/ number 10)) 2))))	       
	       (return-from 20-to-99 "Enter a number between 20 and 99")))
		   
(defun 0-to-99 (number)
		(if (or (< number 0) (> number 99)) (return-from 0-to-99 "Enter a number between 0 and 99") )
	   (if (and (>= number 0) (<= number 9)) (return-from 0-to-99 (0-to-9 number)))
	   (if (and (>= number 10) (<= number 19)) (return-from 0-to-99 (10-to-19 number)))
	   (if (and (>= number 20) (<= number 99)) (return-from 0-to-99 (20-to-99 number))))		   
	   
(defun 100-to-999 (number)
	   (if (or (< number 100) (> number 999)) (return-from 100-to-999 "Enter a number between 100 and 999"))
	   (let ((d (floor (/ number 100)))
		 (c (mod number 100)))
	     (if (= c 0)
		 (return-from 100-to-999 (concatenate 'string (0-to-99 d) " hundred"))
		 (return-from 100-to-999 (concatenate 'string (0-to-99 d) " hundred " (0-to-99 c))))))