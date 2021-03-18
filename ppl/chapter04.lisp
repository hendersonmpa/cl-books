(defun match2 (pat item alist)
  "Returns the association list if match successful else returns $FAIL"
  (cond ((fail-p alist) alist)
        ((is-var pat) (check-add-binding pat item alist))
        ;; pat is a variable, It matches item provided
        ;; 1) it does not have a value earlier, or
        ;; 2) its earlier value is same as item.
        ((and (atom pat) (atom item))
         (cond ((eql pat item) alist)
               (t '$fail)))
        ((or (atom pat) (atom item)) '$fail)
        (t (match2 (rest pat)
                   (rest item)
                   (match2 (first pat)
                           (first item)
                           alist)))))

(defun fail-p (l) (eql '$fail l))

(set-macro-character #\?
                     #'(lambda (stream char)
                         (list '$var (read stream))) t)

(defun is-var (v) (and (not (atom v)) (eql (first v) '$var)))

(defun name-of (v) (second v))

(defun check-add-binding (var val alist)
  (let ((old-binding (assoc (name-of var) alist)))
    (cond ((null old-binding)
           (cons (list (name-of var) val) alist))
          ((equal (second old-binding) val) alist)
          (t '$fail))))

(match2 '(travel (source ?x) (dest ?x) (dist ?y))
        '(travel (source delhi) (dest delhi) (dist 100)) ())

;; Query a database

(defvar *db* '((parent ram jamir)
              (parent jamir samir)
              (parent samir anita)
              (parent sarah jamir)
              (parent sarah mohan)
              (parent ram mohan)
              (parent mohan sheila)
              (parent mohan kusum)))

(defun query-processor (query)
  (cond
    ((not (eql 'and (car query)))
     (simp-query-processor query *db* nil))
    (t (do ((rem-query (cdr query) (cdr rem-query))
            (alists '(nil)))
           ((null rem-query) alists)
           (setf alists
                 (mapcan #'(lambda (alist)
                             (simp-query-processor
                              (car rem-query)
                              *db* alist))
                         alists))))))

(defun simp-query-processor (simp-query db alist)
  (cond ((null db) nil)
        (t (let ((mlist (match2 simp-query (car db) alist)))
             (cond ((fail-p mlist)
                    (simp-query-processor
                     simp-query (cdr db) alist))
                   (t ;; mlist is a new association list.
                    (cons mlist
                          (simp-query-processor
                           simp-query (cdr db)
                           alist))))))))

(query-processor '(and (parent ?x ?y) (parent ?y samir)))
