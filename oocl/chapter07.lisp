;;pg 223
(probe-file "myfile.data")
(defvar s1 (open "myfile.data" :direction :output))
(probe-file "myfile.data")
(write-char #\A s1)
(terpri s1)
(write-spaces 1 s1)
(write-string "This is a test" s1)

(defvar fact '(defun fact (n) (cond ((zerop n) 1)
                                    (t (* n (fact (1- n)))))))

(print fact s1)
(terpri s1)
(close s1)
(open "myfile.data" :direction :probe)

(defvar s2 (open "myfile.data" :direction :input))
(list (read s2) (read s2) (read s2))
(list (read s2) (read s2) (read s2))
(close s2)

(read-from-string "(a list) (another list)")
(read-from-string "(a list) (another list)" :start 9)
(read-from-string "(a list) (another list)" nil 'eof :start 9)

(setf is (make-string-input-stream "1, 2, 3, 4, 5"))
(read-delimited-list #\, is )

(format nil "~[Gold~;Silver~;Bronze~:;No~] Medal" 2)
(format nil "~{~[Gold~;Silver~;Bronze~:;No~] Medal~%~}" '(0 1 2 3))
(format nil "~40,1,1,'.<~A~;~A~;~A~>" "alpha" "beta" "omega")

(defun ordinal (n)
  (format nil "~d~[th~;st~;nd~;rd~:;th~]" n (mod n 10)))
