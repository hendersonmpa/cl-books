;; Tree search
(defvar *positions* '(1 2 3 4) "Rows on the chess board")

(defun no-duplicates (s)
  (cond ((null s) t)
        ((member (car s) (cdr s)) nil)
        (t (no-duplicates (cdr s)))))

(defun badp (board)
  (not (and
        ;; Test for two queens on the same row
        (no-duplicates board)
        ;; Test for two queens on the same NE-SW diag
        (no-duplicates (mapcar #'+
                               *positions*
                               board))
        ;; Test for NW-SE diagonals
        (no-duplicates (mapcar #'-
                               *positions*
                               board)))))

(defun goalp (board)
  (eql 4 (length board)))

(defun successors (board)
  (cond ((eql 4 (length board)) nil)
        (t (mapcar #'(lambda (n) (append board (list n)))
                   *positions*))))

(defun search-df (start)
  (do ((open (list start)
             (append (remove-if #'badp
                                (successors (car open)))
                     (cdr open))))
      ((null open)) ;failure to find goal
    (cond ((goalp (car open)) (return (car open))))))

;(search-df ())
;; Graph Search
(defun remove-bad-prev-visited (l badpred closed)
"Returns a list of nodes in l that are not bad and are not in the
closed list"
(cond ((null l) l)
      ((or (funcall badpred (car l))
           (member (car l) closed :test #'equal))
       (remove-bad-prev-visited
        (cdr l) badpred closed))
      (t (cons (car l)
               (remove-bad-prev-visited
                (cdr l) badpred closed)))))

(defun search-graph-df (start)
  (do ((open (list start)
             (append (remove-bad-prev-visited
                      (successors (car open))
                      #'badp closed)
                     (cdr open)))
       (closed () (cons (car open) closed)))
      ((null open)) ;search failed
    (cond ((goalp (car open))
           (return (car open))))))

(defvar *arcs* nil "Node property list")
(setf (get 'a *arcs*) '(b c d))
(setf (get 'b *arcs*) '(e f))
(setf (get 'f *arcs*) '(i))
(setf (get 'c *arcs*) '(g))
(setf (get 'g *arcs*) '(i))
(setf (get 'd *arcs*) '(g h))
(setf (get 'h *arcs*) '(j k))
(setf (get 'i *arcs*) '(a))

(defun successors (node) (get node *arcs*))
(defun badp (node) (eql node 'e))
(defun goalp (node) (eql node 'j))

(search-graph-df 'a)


;; Least-cost path
(defstruct (entry (:constructor make-entry (node cost predec)))
  node cost predec)

(defun get-path (pair closed)
"Contructs the path (that is, a sequence of nodes) given the last edge
'pair and a set of edges in 'closed"
  (cond ((null pair) ())
        (t (cons (entry-node pair)
                 (get-path (assoc (entry-predec pair)
                                  closed
                                  :test #'equal)
                           closed)))))

(defun searchpath-least (start)
"In each iteration sort the successors and merge with tail of open
using 'entry-cost' as the key"
  (do ((open (list (make-entry start 0 ()))
             (merge 'list
                    (sort (process-successors
                           (successors
                            (entry-node (car open)))
                           (entry-node (car open))
                           (entry-cost (car open))
                           closed)
                          #'<
                          :key #'entry-cost)
                    (cdr open)
                    #'<
                    :key #'entry-cost))
       (closed () (cons (car open) closed)))
      ((null open))
    (cond ((goalp (entry-node (car open)))
           (return (get-path (car open) closed))))))

(defun process-successors (l predec cost closed)
  "l is a list of pairs: successor node and cost of an edge. This
removes those pairs in l that are bad. If a node in l is also on
'closed with a higher cost, the cost in 'closed is updated. Cost of
its successors are also updated.  Finally a list of 'entry is
returned, each 'entry consisting of a good node in l together with its
new cost and predecessor 'predec."
  (cond ((null l) ())
        ((badp (entry-node (car l))) ;drop (car l)
         (process-successors (cdr l) predec cost closed))
        (t (let ((x (assoc (entry-node (car l))
                           closed
                           :test #'equal
                           :key #'entry-node)))
             (cond ((null x) ;(car l) is neither bad nor on closed
                    (cons (make-entry
                           (entry-node (car l))
                           (+ cost (entry-cost (car l)))
                           predec)
                          (process-successors
                           (cdr l) predec cost closed)))
                   ((< (entry-cost x)
                       (+ cost (entry-cost (car l))))
                    ;q; Earlier path on 'closed is cheaper than the
                    ;; current path. Drop (car l)
                    (process-successors (cdr l) predec cost closed))
                   (t ;; Earlier path is costlier. Put current path on
                      ;; the list to be returned
                    (cons (make-entry
                           (entry-node (car l))
                           (+ cost (entry-cost (car l)))
                           predec)
                          (process-successors
                           (cdr l) predec
                           cost closed))))))))

(defun process-successors-mod (l predec cost closed)
  "l is a list of pairs: successor node and cost of an edge. This
removes those pairs in l that are bad. If a node in l is also on
'closed with a higher cost, the cost in 'closed is updated. Cost of
its successors are also updated.  Finally a list of 'entry is
returned, each 'entry consisting of a good node in l together with its
new cost and predecessor 'predec."
  (cond ((null l) ())
        ((badp (entry-node (car l))) ;drop (car l)
         (process-successors-mod (cdr l) predec cost closed))
        (t (let ((x (assoc (entry-node (car l))
                           closed
                           :test #'equal
                           :key #'entry-node)))
             (cond ((null x) ;(car l) is neither bad nor on closed
                    (cons (make-entry
                           (entry-node (car l))
                           (+ cost (entry-cost (car l)))
                           predec)
                          (process-successors-mod
                           (cdr l) predec cost closed)))
                   ((< (entry-cost x)
                       (+ cost (entry-cost (car l))))
                    ;q; Earlier path on 'closed is cheaper than the
                    ;; current path. Drop (car l)
                    (process-successors-mod (cdr l) predec cost closed))
                   (t ;; Earlier path is costlier. Put current path on
                      ;; the list to be returned
                    (cons (make-entry
                           (entry-node (car l))
                           (+ cost (entry-cost (car l)))
                           predec)
                          (process-successors-mod
                           (cdr l) predec
                           cost closed))))))))

(defvar *arcs* nil "Node property list")
(setf (get 'a *arcs*) '((b 3) (c 9) (d 12)))
(setf (get 'b *arcs*) '((e 2) (f 4)))
(setf (get 'f *arcs*) '((i 1)))
(setf (get 'c *arcs*) '((g 1)))
(setf (get 'g *arcs*) '((i 3)))
(setf (get 'd *arcs*) '((g 1) (h 2)))
(setf (get 'h *arcs*) '((j 5) (k 2)))
(setf (get 'i *arcs*) '((a 1)))

(defun successors (node) (get node *arcs*))
(defun badp (node) (eql node 'e))
(defun goalp (node) (eql node 'j))

(searchpath-least 'a)
