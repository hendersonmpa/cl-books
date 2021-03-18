;; 5.7.7

(let ((a+count 0)
      (a+args nil))
  (defun a+ (&rest b)
    (push b a+args)
    (incf a+count)
    (apply #'+ b))
  (defun count+ ()
    (let ((c a+count))
      (setf a+count 0)
      c))
  (defun args+ ()
    (let ((a a+args))
      (setf a+args nil)
      a)))

(defun (setf my-last) (value object)
  (cond ((eq (last object) (cdr object))
         (setf (cdr object) value))
        (t
         (setf (my-last (cdr object)) value))))

(setf independance-day '(1 7 2001))
(last independance-day)
(setf (my-last independance-day) '(2003))
