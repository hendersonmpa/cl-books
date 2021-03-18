

(setf set '((a b c) (c d a) (f b d) (g)))

(reduce #'union set)

(defun total-len (x)
  (reduce #'+ (mapcar #'length x)))

;; pg 221

(setf database '((b1 shape brick)
                 (b1 color green)
                 (b1 size small)
                 (b1 supported-by b2)
                 (b1 supported-by b3)
                 (b2 shape brick)
                 (b2 color red)
                 (b2 size small)
                 (b2 supports b1)
                 (b2 left-of b3)
                 (b3 shape brick)
                 (b3 colour red)
                 (b3 size small)
                 (b3 supports b1)
                 (b3 righ-of b2)
                 (b4 shape pyramid)
                 (b4 color blue)
                 (b4 size large)
                 (b4 supported-by b5)
                 (b5 shape cube)
                 (b5 color green)
                 (b5 size large)
                 (b5 supports b4)
                 (b6 shape brick)
                 (b6 color purple)
                 (b6 size large)
                  ))

(defun match-element ( x pat)
  ( or ( equal x pat)
       ( equal pat '?)))

(defun match-triple ( x pat)
  ( every #'match-element x pat))

(defun fetch (pat)
  (remove-if-not #'(lambda (x) (match-triple x pat))
   database))

(defun color-query (name)
  (cons name '(color ?)))

(defun supporters (name)
  (mapcar #'car
          ( fetch( list '? 'supports name))))

(defun supp-cube (name)
  (member 'cube
          (mapcar
           #'(lambda (b) (third (first (fetch (list b 'shape '?)))))
           (supporters name))))

(defun desc1 (name)
  (fetch (list name '? '?)))

(defun desc2 (name)
  (mapcar #'cdr (desc1 name)))

(defun description (name)
  (reduce #'append (desc2 name)))
