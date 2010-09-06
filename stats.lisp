
(labels ((sum-rec (s l)
	   (if (not l)
	     s
	     (sum-rec (+ s (car l)) (cdr l)))))
  (defun sum (l) (sum-rec 0 l)))

(defun sqr (x)
  (* x x))

(defun sum-squares (l)
  (sum (mapcar #'sqr l)))

(defun average (x)
  (float (/ (sum x) (length x))))

(defun std-dev (x)
  (let ((n (length x))
	(sx (sum x))
	(sx2 (sum-squares x)))
    (sqrt (* (/ n (- n 1)) (- (/ sx2 n) (sqr (/ sx n)))))))

(defun dumpit (x)
  (format t "Length: ~A~%" (length x))
  (format t "Sum: ~A~%" (sum x))
  (format t "Sum of squares: ~A~%" (sum-squares x))
  (format t "Average: ~5$~%" (average x))
  (format t "Standard deviation: ~5$~%" (std-dev x)))

