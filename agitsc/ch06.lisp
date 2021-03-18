;; Page `78
(setf description '(large red shiny cube -vs- small shiny red four-sided pyramid))

(defun right-sided (l)
  (cdr (member '-vs- l)))

(defun left-sided (l)
  (cdr (member '-vs- (reverse l))))

(defun count-common (l)
  (length ( intersection (left-sided l) (right-sided l))))

(defun compare (l)
  (cons (count-common l) '(common features)))

;; pg 184

(setf nerd-states
      '( (sleeping  eating)
        (eating  waiting)
        (waiting  programming)
        (programming  debugging)
        (debugging  sleeping)))

(defun nerdus (x)
  (cadr (assoc x nerd-states)))

(defun sleepless-nerd (x)
  (let ((y (nerdus x)))
    (if (equal y 'sleeping)
        (nerdus y)
        y)))

(defun nerd-on-caffeine (x)
  (nerdus (nerdus x)))
