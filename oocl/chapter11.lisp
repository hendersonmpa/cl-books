;; macro characters pg 394
(defvar my-readtable
  (copy-readtable *readtable*))

(defun new-readtable (new)
  (if (not (readtablep new))
      (error "new-readtable: Bad readtable: ~A" new))
  (setf *old-readtable* *readtable*)
  (setf *readtable* new))

(defun old-readtable ()
  (setf *readtable* *old-readtable*))

(new-readtable my-readtable)

(set-macro-character #\[
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (apply #'+ (read-delimited-list #\] stream t)))
                     t my-readtable)

(set-macro-character #\] (get-macro-character #\) nil))

;; > [1 2 [12] 3 5 6 7] ===> 36

(old-readtable)


;; Question 11.10.2

(setf f '(1 2 3) g '(4 5 6))

(defmacro swap (a b)
  `(let ((old-a ,a))
     (setf ,a ,b ,b old-a)))

(defmacro swap2 (x y)
  `(let ((tmp ,x))
     (setf ,x ,y)
     (setf ,y tmp)))

(defmacro repeat (times &body body)
  (let ((x (gensym)))
    `(dotimes (,x ,times)
       ,@body)))

(defun repeat-char (stream char count)
  (repeat count char (values)
          (write-char char stream)))

(defun mrepeat-char (stream char count)
  (let ())
  `(repeat ,count ,char (values)
          (write-char ,char ,stream)))
