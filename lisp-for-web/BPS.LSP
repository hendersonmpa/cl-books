;;
 ; File: BPS.Lisp
 ;
 ; Simple backward-chaining production system interpreter
 ;    Copyright 1990 by Mark Watson
 ;;

(setq *BT-DEBUG* T)  ;; flag for debug output

(defun backward (ruleList FactList Goal inferenceChain)
  ;;
   ;  Test for desired debug printout:
   ;;
  (if *BT-DEBUG* (terpri))
  (if *BT-DEBUG* 
    (pprint (list "Entering backward: ruleList=" ruleList "FactList=" FactList
                  "Goal=" Goal "inferenceChain=" inferenceChain)))
  ;;
   ;  Test having already found the path from the goal
   ;  back to the initial state of the system:
   ;;
  (if (member Goal FactList :test #'equal)
    inferenceChain ;; done with search
    ;; Find a rule with the then clause equal to the goal
    ;; and backtrack trying to prove the if clause in the rule:
    (dolist (rule ruleList)
      (if (equal (cadr rule) Goal)   ;; (cadr rule) equals the then clause in the rule
        (if (member (car rule) FactList :test #'equal)
          (pprint (list "Solution to backtracking problem:"
                        (append (append FactList  (list Goal))
                                inferenceChain)))
           ;;
            ;  Recursively call this function backward passing the if clause
            ;  of the current rule being processed as the goal to search
            ;  backwards from and append the current goal to the inference chain:
            ;;
          (let ((search (backward ruleList FactList (car rule) 
                                  (cons Goal inferenceChain))))
            (if search
              (return (cons (list rule FactList) search)))))))))

;;
 ;Test data. This example rule set will automatically execute when
 ; this sample file is loaded into your Common Lisp environment:
 ;;
(setq rules '(
                     ((A) (B))    ;; if A then B
                     ((B) (C))    ;; if B then C
                     ((C) (X))    ;; if C then X-not used in inference chain
                     ((Z) (D))    ;; if Z then D-not used in inference chain
                     ((C) (D))    ;; if C then D
                     ((D) (E))    ;; if D then E
                     ((E) (F))))  ;; if E then F

(setq facts '((A)))
(setq goal '(F))

;;
 ; Run the system:
 ;;

(setq *BT-DEBUG* nil)  ;; turn off debug output

(progn
  (terpri) (terpri) (princ "********************")
  (terpri) 
  (terpri)
  (backward rules facts goal nil))