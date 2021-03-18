;; Closures

(defun gen-sumsq (sumfn sqfn)
  #'(lambda (x y)
      (funcall sumfn (funcall sqfn x)
               (funcall sqfn y))))

(defun f (m n)
  (labels ((sq (x) (* x x)))
    (let ((sumsq (gen-sumsq #'+
                            #'sq)))
      (funcall sumsq m n))))
