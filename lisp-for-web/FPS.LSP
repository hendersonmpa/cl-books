;;
 ; File: FPS.LISP
 ;
 ; Simple forward-chaining production system interpreter (without variables)
 ;
 ;  Copyright 1990 Mark Watson.
 ;;

(defun forward (ruleList FactList)
  (let ((keep-going t))
    (loop
      (if (not keep-going) (return))
      (dolist (rule ruleList)
        (setq keep-going nil)
        (and
         (member (car rule) FactList :test #'equal)
         (not-fact (cadr rule) FactList)
         (progn
           (princ "Next rule to execute: ")
           (print rule)
           (terpri)
           t)
         (setq keep-going t)
         (make-fact (cadr rule) FactList))))))

(defun not-fact (testFact Facts)
  (not (member testFact Facts)))

(defun make-fact (fact Facts)
  (setf (cdr Facts) (cons fact (cdr Facts)))
  (princ "New fact: ")
  (print fact)
  (terpri))