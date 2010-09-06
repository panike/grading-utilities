;; For non leap years
(defun get-num (d)
  (cond ((>= d 335) 12) ((>= d 305) 11) ((>= d 274) 10)
	((>= d 244) 9) ((>= d 213) 8) ((>= d 182) 7)
	((>= d 152) 6) ((>= d 121) 5) ((>= d 91) 4)
	((>= d 60) 3) ((>= d 32) 2) ((>= d 1) 1) (t 0)))

;; Leap years, like 2008
;; (defun get-num (d)
;;   (cond ((>= d 336) 12) ((>= d 306) 11) ((>= d 275) 10)
;; 	((>= d 245) 9) ((>= d 214) 8) ((>= d 183) 7)
;; 	((>= d 153) 6) ((>= d 122) 5) ((>= d 92) 4)
;; 	((>= d 61) 3) ((>= d 32) 2) ((>= d 1) 1) (t 0)))

(defvar *month-nums*)

(setf *month-nums* (make-array 12 :initial-contents '(0 31 59 90 120
 						      151 181 212 243
 						      273 304 334)))
;;(setf *month-nums* (make-array 12 :initial-contents '(0 31 60 91 121
;;						      152 182 213 244
;;						      274 305 335)))

(defun format-date (d)
  (format t "2009~2,'0D~2,'0D~%"
	  (get-num d)
	  (- d (aref *month-nums* (- (get-num d) 1)))))
