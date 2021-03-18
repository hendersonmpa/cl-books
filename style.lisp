;;; Note from Lisp Style and Design
;;;

;;;; Debugging
;; (defun flatten (lis)
;;   (cond ((null lis) nil)
;;         ((atom (car lis)) (cons (car lis) (flatten (cdr lis))))
;;         (t (append (flatten (car lis)) (flatten (cdr lis))))))

;; (setq x (make-array '(3 5) :initial-element 3))
;; (setq y (make-array '(3 5) :initial-element 7))

;; (defun matrix-multiply (a b)
;;   (let ((*print-array* nil))
;;     (assert (and (= (array-rank a) (array-rank b) 2)
;;                  (= (array-dimension a 1) (array-dimension b 0)))
;;             (a b)
;;             "Cannot multiply ~S by ~S." a b)
;;     '(a b))) ; This is where you would multiply the matrices

;; (matrix-multiply x y)

;; (defun double-safely (x)
;;   (assert (numberp x) (x)) (+ x x))

;; (double-safely 4)
;; (double-safely t)

;; (defun type-test (test-arg)
;;   (check-type test-arg (integer) "an integer")
;;   (format t "The argument ~A is an integer." test-arg))


;;;; Efficiency

(proclaim '(optimize (speed 3) (safety 1)
            (space 0) (compilation-speed 0)))

;;; Compare simple strings for equality
(defun compare-simple-strings
    (string1 string2 start1 end1 start2 end2)
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2))
       (values (and (= i1 end1) (= i2 end2)) i1 i2))
    (unless (char= (aref string1 i1) (aref string2 i2))
      (return (values nil i1 i2)))))

(defun fast-compare-simple-strings
    (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 end1 start2 end2))
  (declare (simple-string string1 string2))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2))
       (values (and (= i1 end1) (= i2 end2)) i1 i2))
    (declare (fixnum i1 i2))
    (unless (char= (aref string1 i1) (aref string2 i2))
      (return (values nil i1 i2)))))

;;; Time several iterations of the function
(defun test (iterations function str1 str2 start1 end1 start2 end2)
  (time (dotimes (i iterations)
          (funcall function str1 str2 start1 end1 start2 end2))))

(test 2500000 #'compare-simple-strings "mollly" "llama" 2 3 1 3)
(test 2500000 #'fast-compare-simple-strings "mollly" "llama" 2 3 1 3)
