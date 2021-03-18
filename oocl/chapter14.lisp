;; OOCL Chapter 14 pg 547

(defun check-hash-table-values (ht goodtype)
  (with-hash-table-iterator
      (get-bucket ht)
    (labels
        ((next-bucket
             (bucket &optional key value)
           (when bucket
             (cond ((eq (type-of value) goodtype))
                   (t
                    (format t "~%~A's value (~A) not of type ~A"
                            key value goodtype)))
             (multiple-value-call #'next-bucket (get-bucket)))))
      (multiple-value-call #'next-bucket (get-bucket)))))

(setf age-table (make-hash-table))
(setf (gethash 'joe age-table) 34
      (gethash 'mary age-table) 26
      (gethash 'will age-table) 'twelve)

(type-of 34)
(check-hash-table-values age-table 'integer)
(type-of 'twelve)
(check-hash-table-values age-table 'symbol)
