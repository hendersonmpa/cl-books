(defun foo (x)
  (format t "Paramter: ~a~%" x)
  (let ((x 2) (y 10))
    (format t "Outer LET: ~a: ~a~%" x y)
    (let ((x 3) (y 20) )
      (format t "Inner LET: ~a: ~a~%" x y))
    (format t "Outer LET: ~a: ~a~%" x y))
  (format t "Paramter: ~a~%" x))

(let ((count 0)) #'(lambda () (setf count (1+ count))))
(defparameter *fn* (let ((count 0))
                     #'(lambda () (setf count (1+ count)))))

(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count)))

(defvar golden-ratio nil)

(setf golden-ratio (/ (+ 1 (sqrt 5))2))

(defun dimensions (length ratio)
  (let* (( cm-length ( * length 2.54))
         (cm-width ( / cm-length ratio )))
    (format t  "Length is: ~a cm, Width is: ~a cm"  cm-length cm-width)))
