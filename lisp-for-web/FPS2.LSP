;;
 ; File: FPS2.LISP
 ;
 ; Forward-chaining production system interpreter (with variables)
 ;
 ; Copyright 1990 by Mark Watson
 ;;

(defun forward (ruleList FactList)
  (let ((keep-going t))
    (loop
      (if (not keep-going) (return))
      (dolist (rule ruleList)
        (setq keep-going nil)
        (setq var-list nil) ;; reset current variable binding list
        (and
         (fireRule? rule FactList var-list)
         (progn
           (princ "From rule:")
           (print rule)
           (terpri)
           t)
         (setq keep-going t))))))
;;
 ; Check to see if a new fact has already been added to the fact database:
 ;;

(defun not-fact (testFact Facts)   (not (member testFact Facts :test #'equal)))

;;
 ; Add a new fact to the fact database. Note that this function
 ; alters the value of the second argument with setf.
 ;;

(defun make-fact (fact Facts)
  (if fact ;; make sure that fact is not just nil
    (let ()
      (setf (cdr Facts) (cons fact (cdr Facts)))
      (princ "New fact: ")
      (print fact)
      (terpri))))

;;
 ; Functions for variable matching:
 ;;

(defun isVar? (x)  ;; check to see if the first character in name is a ?
  (and (symbolp x) (char-equal (char (symbol-name x) 0) #\?)))

(defun varMatch (v-list fact var-list) ;; wild-card pattern match
  ;; Make sure the lists are the same length:
  (if (equal (length v-list) (length fact))
    (let ((match-ok t))
      (dolist (v v-list)
        (if (not (equal v (car fact))) ;; no match, but check for a variable:
          (if (isVar? v)
            (setq var-list (cons (list v (car fact)) var-list)
                  fact (cdr fact))
            (return (setq match-ok nil))) ;; no match and no variable
          (setq fact (cdr fact))))
      (if match-ok
        (if var-list ;; we need to return a non-nil value so return the
          var-list   ;; new updated variable list
          T)))))     ;; or return T to signal a match to the calling function.

(defun varMatchAll (v-list theFacts var-list &aux ret)
  (dolist (fact theFacts)
    (setq ret (varMatch v-list fact var-list))
    (if ret
      (if (not (equal ret var-list))
        (return ret)
        ret))))
;;
 ; Test for valid rule-firing. Return updated variable binding list
 ; if the rule is eligible to execute:
 ;;

(defun fireRule? (rule FactList var-list)
  (if (not (equal (car rule) 'if)) ;; simple check on rule syntax
    (let ()
      (princ "Illegal rule:")
      (print rule))
    (let ((valid t) added-fact)
      (setq rule (cdr rule))
      (do ((check (car rule)))  ; check = if clauses
          ((or
            (null rule)
            (equal check 'then)))
          (let (new-var-bindings)
            (setq new-var-bindings
                  (varMatchAll check FactList var-list))
            (if new-var-bindings
              (if (not (equal new-var-bindings T))
                (setq var-list (append new-var-bindings var-list)))
              (return (setq valid nil))))  ;; if clause that does not match
          (setq rule (cdr rule)) ;; this line is missing in listing in the book (sorry!)
          (setq check (car rule)))
      (if (equal (car rule) 'then)
        (if valid ;; execute the then clauses of the rule:
          (let (new-fact)
            (setq rule (cdr rule))
            (dolist (thenClause rule)
              (setq new-fact (substituteVariables thenClause var-list))
              (if (not-fact new-fact FactList)
                (let ()
                     ;; Surgically insert new-fact
                  (make-fact new-fact FactList)
                  (setq added-fact t))))
            added-fact))))))

;;
 ; Use the variable binding list to substitute values for variables:
 ;;
(defun substituteVariables (fact var-list)
  (dolist (var var-list)
    (let ((variable-name (car var))
          (bound-value (cadr var)))
      (setq fact
            (mapcar
             #'(lambda (x)
                 (if (equal x variable-name)
                   bound-value
                   x))
             fact))))
  fact)

;;
 ; Test data:
 ;;

(setq rules '(
              (if (in jeep) (has ?object) then (drive jeep))
              (if (has ?x) then (drop ?x))
              (if (drive jeep) then (be-at store))
              (if (in jeep) then (leave jeep))
              (if (leave jeep) then (walk))))

(setq facts '(  (in jeep)   (has keys)))

;;
 ; Test execution of the system automatically as this file is loaded:
 ;;

(forward rules facts)
