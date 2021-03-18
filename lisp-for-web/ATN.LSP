;; 
 ;File: ATN.LISP
 ;
 ;  Copyright 1990 by Mark Watson.
 ;;

;;
 ; Word descriminators:
 ;;

(defun isNoun? (word)
  (if (member word *nouns*)
    (setq *last-noun* word)))

(defun isVerb? (word)
  (member word *verbs*))

(defun isAdj? (word)
  (member word *adjs*))

(defun isPrep? (word)
  (member word *preps*))

;;
 ; Parsing networks:
 ;;

(defun np? ()
  (let ((save *sentence*))
    (if
      (or
       (and
        (word? 'adj)
        (or (np?) (word? 'noun)))
       (word? 'noun))
      (let ()
        (if (null *subject*) (setq *subject* *last-noun*))
        t)
      (let ()
        (setq *sentence* save)
        nil))))

(defun prepp? ()
  (let ((save *sentence*))
    (if (and
         (word? 'prep)
         (np?))
      t
      (let ()
        (setq *sentence* save)
        nil))))

(defun vp? ()
  (let ((save *sentence*))
    (if (or
         (and (word? 'verb) (np?))
         (let ()
           (setq *sentence* save)
           (word? 'verb)))
      t
      (let ()
        (setq *sentence* save)
        nil))))

(defun sentence? ()
  (let ((save *sentence*))
    (if (or
         (and
          (np?)
          (vp?)
          (prepp?))
         (let ()
           (setq *sentence* save)
           (and (np?)
                (vp?))))
      t
      (let ()
        (setq *sentence* save)
        nil))))

;;
 ; Test for word type:
 ;;

(defun word? (type)
  (let ((save *sentence*)
        (w (if (null *sentence*) nil (car *sentence*)))
        (func
         (assoc
          type
          '((noun isNoun?) (verb isVerb?) (prep isPrep?) (adj isAdj?)))))
    (if func
      (if (funcall (cadr func) w)
        (let ()
          (setq *sentence* (cdr *sentence*))
          t)
        nil)
      nil)))

;;
 ; Test words:
 ;;

(setq *nouns* '(block dog I))
(setq *verbs* '(grab see))
(setq *adjs* '(blue green large red the))
(setq *preps* '(behind under))

(setq *subject* nil)
      
;;
 ; Test sentence:
 ;;

(setq *sentence* '(I see the large red block under the large dog))

;;
 ; Turn on tracing:
 ;;
(trace isNoun? isVerb? isAdj? isPrep? np? prepp? vp? sentence?)

;;
 ; Run system:
 ;;
(sentence?)