;; pg 243

(defun laugh (n)
  (cond  (( zerop n) nil)
  (t ( cons 'ha ( laugh (- n 1))))))

(defun add-up (l)
  (cond ((null l) 0)
        (t (+  (first l) ( add-up (rest l))))))

(defun alloddp (x)
  (cond ((null x)t)
        (t ( and (oddp  (first x)) (alloddp (rest x))))))

(defun alloddp2 (x)
  (cond ((null x) t)
        ((evenp (first x)) nil)
        (t (alloddp (rest x)))))

;; pg 254

(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))

(defun my-nth (n x)
  (cond ((zerop n) (first x))
        ((null x ) nil)
        (t (my-nth (- n 1) (rest x )))))

(defun sum-numeric (x)
  (cond ((null x) 0)
        ((numberp  (first x))
         (+ (first x )
            (sum-numeric (rest x))))
        (t (sum-numeric (rest x)))))

;; pg 272
(defun fact (n)
  (cond ((zerop n) (break "N is zero."))
        (t (* n (fact (- n 1))))))

(defvar family
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)
        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)
        (george frank linda)
        (hillary frank linda)
        (andre nil nil)
        (tamara bruce suzanne)
        (vincent bruce suzanne)
        (wanda nil nil)
        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)
        (nigel andre hillary)
        (frederick nil tamara)
        (zelda vincent wanda)
        (joshua ivan wanda)
        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)
        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

(defun father (name)
  (second (assoc name family)))

(defun mother (name)
  (third (assoc name family)))

(defun parents (name)
  (union (and (father name) (list (father name)))
           (and (mother name) (list (mother name)))))

(defun children (parent)
  (and parent
       (mapcar #'first
               (remove-if-not
                #'(lambda (entry)
                    (member parent (rest entry)))
                family))))
(defun siblings (x)
  (set-difference (union (children (father x))
                         (children (mother x)))
                  (list x)))

(defun mapunion (fn x)
  (and x (reduce #'union (mapcar fn x))))

(defun grandparents (x)
  (mapunion #'parents (parents x)))

(defun cousins (x)
  (mapunion #'children
            (mapunion #'siblings (parents x))))

(defun descended-from (p1 p2)
  (cond ((null p1) nil)
        ((member p2 (parents p1)) t)
        (t (or (descended-from
                (father p1) p2)
               (descended-from
                (mother p1) p2)))))

(defun ancestors (x)
  (cond ((null x) nil)
        (t (union
            (parents x)
            (union (ancestors (father x))
                   (ancestors (mother x)))))))

(defun generation-gap (x y)
  (g-gap-helper x y 0))

(defun g-gap-helper (x y n)
  (cond ((null x) nil)
        ((equal x y) n)
        (t (or (g-gap-helper
                (father x) y (1+ n))
               (g-gap-helper
                 (mother x) y (1+ n))))))
