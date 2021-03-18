(defun add-q (n)
  (+ n q))

(let ((q 1))
  (declare (special q))
  (add-q 10))


(let ((q 1))
    (add-q 10))
