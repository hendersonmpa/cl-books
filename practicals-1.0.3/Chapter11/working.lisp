;; Collections

(setf sequence '(1 2 1 3 1 2 3 4))
(remove-duplicates sequence)
(setf s-seq (sort sequence #'<))
(setf s-seq (sort (remove-duplicates sequence) #'<))

(merge 'list sequence s-seq #'<)
(concatenate 'list sequence s-seq)

(defparameter *h* (make-hash-table))

(setf (gethash 'foo *h*) 'quux)
(gethash 'foo *h*)

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format nil "Value ~a actually present." value)
        (format nil "Value ~a because key not found." value))))

(setf (gethash 'bar *h*) nil)
(show-value 'foo *h*)
(show-value 'bar *h*)
(show-value 'baz *h*)

(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
