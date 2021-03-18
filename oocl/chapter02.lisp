;; Zeller's Congruence
;; d = (N + [2.6M - 0.2] + Y + [Y/4] + [C/4] - 2C - (1 + L)*[M/11]) mod7

(defun zeller (N M C Y L)
  (mod (+ N Y
          (truncate (- (* 13/5 M) 0.2))
          (truncate (/ Y 4))
          (truncate (/ C 4))
          (- (* 2 C))
          (- (* (+ 1 L)
                (truncate (/ M 11)))))
       7))


;; The year is divisible by 4 and not divisible by 100 unless divisible by 400
(defun leap-yearp (year)
  (cond ((zerop (rem year 400))
         (print "Leap Year"))
        ((zerop (rem year 100))
         nil)
        ((zerop (rem year 4))
         (print "Leap Year"))
        (t nil)))


(defun son-of-zeller (day month year)
  (zeller day month (truncate year 100) (rem year 100)
          (cond ((leap-yearp year) 1)
                (t 0))))
