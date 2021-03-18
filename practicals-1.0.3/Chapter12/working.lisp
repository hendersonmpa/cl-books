(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(defun upto-2 (max)
  (let ((result nil))
    (dotimes (i max)
      (push (1+ i) result))
    (nreverse result)))

(setf *x* '(1 2 3 4 5 6 7 8))
(= (elt *x* 3)(nth 3 *x*))


(setf l (list 1 2 3))
(setf l2 (list (list 1 2) (list 3 4)))
(car l2)
(cdr l2)
(cdr l)
(cadr l2)
(cddr l2)
(caadr l2)






(dotimes (x 8)
  (dotimes (y 8)
    (if (evenp (+ x y))
        (setf (aref board x y) :black)
        (setf (aref board x y) :white))))


(setf color-cube (make-array '(3 3 3)))
(array-rank color-cube)
(array-total-size color-cube)
(array-dimensions color-cube)

    "this dotimes is an iterator which loops x over integers 0 to 7"


(dotimes (x 3)
      (dotimes (y 3)
        (dotimes (z 3)
          (if (= x 0)
              (setf (aref color-cube x y z) :red)
              (setf (aref color-cube x y z) :white)
              ))))

(defvar x)
(setf x (make-array '(2 2) :initial-element 5 :adjustable t))
(adjust-array x '(3 4))

(setf x (make-array '(4) :initial-contents '(:one :two :three :four)))
(array-dimensions x)
(aref x 0)
