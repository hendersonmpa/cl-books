
(defun add_rules_to_expand (goal vertex)
  (dolist (rule rules)
    (let
      ((subst_RHS (lhs_match goal rule)))
      (if (not (null (car subst_RHS)))
        (let* (
           (LHS (apply_subst (car subst_RHS) goal))
           (RHS (cadr subst_RHS)))
          (if (not (subsumed_goal LHS vertex))
            ;; if goal not subsumed, add the new edge,
            ;; renaming so that it can combine with other
            ;; edges from rules using the same variable
            ;; symbols (e.g. edges from the same rule that just
              ;; produced it)
            (agenda_add
              (rename
                (list
                  vertex
                  vertex
                  LHS
                  nil
                  RHS))))))))
  (record_goal (goal vertex)))

;;; look to see whether a particular category, or one more
;;; general than it, has been looked for at a given vertex

(defun subsumed_goal (goal vertex)
  (let ((goaldag
       (list
         (list 'CAT goal)
         (list 'START vertex)
         (list '& (newvar)))))
    (dolist (d existing_goals nil)
      (if (subsumes g goaldag)
        (return t)))))

;;; record that a particular category has been searched for
;;; at a given vertex

(defun record_goal (goal vertex)
  (setq existing_goals
    (cons
      (list
        (list 'CAT goal
          (list 'START vertex)
          (list '& (newvar))))
      existing_goals)))

TYPE=1, ITEMS=94, ITEMBYTES=1, PUB
(defun active_edge_function (edge)
  (add_rules_to_expand (car (tofind edge)) (finish edge)))
