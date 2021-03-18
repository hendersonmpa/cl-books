;;; tests for LISP programs

;;; FSRECOG

(lib 'fsrecog)
(lib 'english1)

(recognize english1 '(kim was happy))     ;;; T
(recognize english1 '(lee is a consumer and often very very stupid)) ;;; T
(recognize english1 '(sandy was sometimes a stupid man and kim is
  always the happy woman))                    ;;; T
(recognize english1 '(a happy consumer is sometimes stupid)) ;;; NIL

;;; FSGENERATE

(lib 'fsgen)
(lib 'english1)
(generate english1)    ;;; infinite loop

(setq english1
'(
    (initial (1))
    (final (9))
    (from 1 to 3 by NP)
    (from 1 to 2 by DET)
    (from 2 to 3 by N)
    (from 3 to 4 by BV)
    (from 4 to 5 by ADV)
    (from 4 to 5 by |#|)
    (from 5 to 6 by DET)
    (from 5 to 7 by DET)
    (from 5 to 8 by |#|)
    (from 6 to 7 by ADJ)
    (from 6 to 6 by MOD)
    (from 7 to 9 by N)
    (from 8 to 8 by MOD)
    (from 8 to 9 by ADJ)
    (from 9 to 4 by CNJ)
    (from 9 to 1 by CNJ)
    )
)

(generate english1)
/*
(LEE WAS SOMETIMES HER STUPID WOMAN)
(LEE WAS SOMETIMES HER STUPID MAN)
(LEE WAS SOMETIMES HER STUPID CONSUMER)
(LEE WAS SOMETIMES HER HAPPY WOMAN)
(LEE WAS SOMETIMES HER HAPPY MAN)
(LEE WAS SOMETIMES HER HAPPY CONSUMER)
(LEE WAS SOMETIMES HER VERY STUPID WOMAN)
(LEE WAS SOMETIMES HER VERY STUPID MAN)
(LEE WAS SOMETIMES HER VERY STUPID CONSUMER)
(LEE WAS SOMETIMES HER VERY HAPPY WOMAN)
(LEE WAS SOMETIMES HER VERY HAPPY MAN)
(LEE WAS SOMETIMES HER VERY HAPPY CONSUMER)
(LEE WAS SOMETIMES HER VERY VERY STUPID WOMAN)
*/

;;; FSBGENERATE

(lib 'fsbgen)
(lib 'english1)

(generate english1)
/*
(KIM IS HAPPY)
(KIM IS STUPID)
(KIM IS A CONSUMER)
(KIM IS A MAN)
(KIM IS A WOMAN)
(KIM IS THE CONSUMER)
(KIM IS THE MAN)
(KIM IS THE WOMAN)
(KIM IS HER CONSUMER)
(KIM IS HER MAN)
(KIM IS HER WOMAN)
(KIM IS OFTEN HAPPY)
(KIM IS OFTEN STUPID)
(KIM IS OFTEN A CONSUMER)
(KIM IS OFTEN A MAN)
(KIM IS OFTEN A WOMAN)
(KIM IS OFTEN THE CONSUMER)
(KIM IS OFTEN THE MAN)
(KIM IS OFTEN THE WOMAN)
(KIM IS OFTEN HER CONSUMER)
(KIM IS OFTEN HER MAN)
*/

;;; FSTRANSDUCE

(lib 'fstrans)
(lib 'eng_fre1)

(transduce eng_fre1 '(where is the toilet))   ;;; (OU EST LA TOILETTE) T

;;; FSGENERATE2

(lib 'fstgen)
(lib 'eng_fre1)

(generate2 eng_fre1)
   ;;; ((WHERE IS THE EXIT) (OU EST LA_SORTIE))
   ;;; ((WHERE IS THE POLICEMAN) (OU EST LE_GENDARME))
   ;;; ((WHERE IS THE SHOP) (OU EST LA_BOUTIQUE))
   ;;; ((WHERE IS THE TOILET) (OU EST LA_TOILETTE)) T

;;; RECRECOG

(lib 'rtnrecog)
(lib 'english2)

(rtn_recognize 'S '(the woman who sings sees that Mayumi lacks a house))   ;;; T

;;; RECTRANSDUCE

(lib 'pdtransd)
(lib 'eng_fre2)

(rtn_transduce 'S '(the man who sings sees that Mary lacks a house))
;;; (LE HOMME QUI CHANTE VOIT QUE MARIE MANQUE UNE MAISON)

;;; ATN, ATNMOD

(lib 'atnrecog)
(lib 'atnarcs1)

(atn_recognize 'S '(john will see peter behind susan))
;;; (ADD (SEE (ARG0 JOHN) (ARG1 PETER) (BEHIND SUSAN)))

(lib 'atnrecog)
(lib 'atnarcs2)

(atn_recognize 'S '(who will john see peter behind))
;;; (SEARCH (SEE (ARG0 JOHN) (ARG1 PETER) (BEHIND |?|)))

;;; RANDGEN, CFGRAM

(lib 'cfpsgram)
(lib 'randgen)

(generate '(s))
(generate '(s))
(generate '(s))
(generate '(s))
(generate '(s))
   ;;; (NURSES APPEARED)
   ;;; (DR CHAN APPROVED)
   ;;; (DR CHAN BELIEVED)
   ;;; (NURSES EMPLOYED NURSES)
   ;;; (DR CHAN DIED)

;;; RANDTREEGEN, CFGRAM

(lib 'cfpsgram)
(lib 'randtree)

(generate '(s))
(generate '(s))
(generate '(s))
(generate '(s))
(generate '(s))
   ;;; (S (NP MEDICENTER) (VP (V EMPLOYED)))
   ;;; (S (NP DR CHAN) (VP (V APPROVED) (NP JOHN)))
   ;;; (S (NP NURSES) (VP (V APPROVED) (NP DR CHAN)))
   ;;; (S (NP JOHN) (VP (V EMPLOYED)))
   ;;; (S (NP NURSES) (VP (V DIED) (NP NURSES)))

;;; FEATEXPAND

(lib 'featexp)

 (setq features
  '(
    (cat S NP VP PP AP P N V 0)
    (slash S NP VP PP AP P N V 0)
    (subcat S NP VP PP AP P N V 0)
    (empty yes no)
   )
 )

(setq rules
'(
( ((cat S) (slash _x))
  ((cat NP) (slash 0))
  ((cat VP) (slash _x)) )
( ((cat VP) (slash _y))
  ((cat V) (subcat _x) (slash 0))
  ((cat _x) (slash _y)) )
( ((cat PP) (slash _y))
  ((cat P) (subcat _x) (slash 0))
  ((cat _x) (slash _y)) )
( ((cat S) (slash 0))
  ((cat _x) (slash 0) (empty no))
  ((cat S) (slash _x) (empty no)) )
( ((cat _x) (slash _x) (empty yes)) )
))

(mapc #'expand_rule rules)
/*
((YES S S S) (YES S 0 NP) (YES S S VP))
((YES S NP S) (YES S 0 NP) (YES S NP VP))
((YES S VP S) (YES S 0 NP) (YES S VP VP))
((YES S PP S) (YES S 0 NP) (YES S PP VP))
((YES S AP S) (YES S 0 NP) (YES S AP VP))
((YES S P S) (YES S 0 NP) (YES S P VP))
((YES S N S) (YES S 0 NP) (YES S N VP))
((YES S V S) (YES S 0 NP) (YES S V VP))
((YES S 0 S) (YES S 0 NP) (YES S 0 VP))
((YES NP S S) (YES S 0 NP) (YES S S VP))
((YES NP NP S) (YES S 0 NP) (YES S NP VP))
((YES NP VP S) (YES S 0 NP) (YES S VP VP))
((YES NP PP S) (YES S 0 NP) (YES S PP VP))
((YES NP AP S) (YES S 0 NP) (YES S AP VP))
((YES NP P S) (YES S 0 NP) (YES S P VP))
((YES NP N S) (YES S 0 NP) (YES S N VP))
((YES NP V S) (YES S 0 NP) (YES S V VP))
((YES NP 0 S) (YES S 0 NP) (YES S 0 VP))
((YES VP S S) (YES S 0 NP) (YES S S VP))
((YES VP NP S) (YES S 0 NP) (YES S NP VP))
((YES VP VP S) (YES S 0 NP) (YES S VP VP))
((YES VP PP S) (YES S 0 NP) (YES S PP VP))
((YES VP AP S) (YES S 0 NP) (YES S AP VP))
((YES VP P S) (YES S 0 NP) (YES S P VP))
((YES VP N S) (YES S 0 NP) (YES S N VP))
((YES VP V S) (YES S 0 NP) (YES S V VP))
((YES VP 0 S) (YES S 0 NP) (YES S 0 VP))
((YES PP S S) (YES S 0 NP) (YES S S VP))
((YES PP NP S) (YES S 0 NP) (YES S NP VP))
((YES PP VP S) (YES S 0 NP) (YES S VP VP))
((YES PP PP S) (YES S 0 NP) (YES S PP VP))
((YES PP AP S) (YES S 0 NP) (YES S AP VP))
((YES PP P S) (YES S 0 NP) (YES S P VP))
((YES PP N S) (YES S 0 NP) (YES S N VP))
((YES PP V S) (YES S 0 NP) (YES S V VP))
((YES PP 0 S) (YES S 0 NP) (YES S 0 VP))
((YES AP S S) (YES S 0 NP) (YES S S VP))
((YES AP NP S) (YES S 0 NP) (YES S NP VP))
*/

;;; BUREC, CFGRAM

(lib 'cfpsgram)
(lib 'burecog)

(next '(Medicenter employed Dr Chan))
(YES)
(YES)
(YES)
(YES)
(YES)
(YES)
(YES)
(YES) NIL

;;; BUPARSE

(lib 'cfpsgram)
(lib 'buparse1)

(parse '(Medicenter employed Dr Chan))
   ;;; ((S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN)))
   ;;;  (S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN)))
   ;;;  (S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN)))
   ;;;  (S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN)))
   ;;;  (S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN)))
   ;;;  (S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN)))
   ;;;  (S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN)))
   ;;;  (S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN))))

;;; BETTERBUPARSE

(lib 'cfpsgram)
(lib 'buparse2)

(parse '(Medicenter employed Dr Chan))
   ;;; ((S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN))))

;;; TDREC

(lib 'cfpsgram)
(lib 'tdrecog)

(next '((S)) '(Medicenter employed Dr Chan)) ;;; (YES) NIL

;;; TDPARSE

(lib 'cfpsgram)
(lib 'tdparse)

(parse '(s) '(Medicenter employed Dr Chan))
   ;;; ((S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN))))

;;; BUBREC

(lib 'cfpsgram)
(lib 'bubrecog)

(recognize '(Medicenter employed Dr Chan))   ;;; T
(S)
(S)
(S)
(S)
(S)
(S)
(S)
(S) NIL

;;; CHART

(lib 'cfpsgram)
(lib 'chart)

(chart_parse '(s) '(Medicenter employed Dr Chan))
   ;;; (((S (NP MEDICENTER) (VP (V BELIEVED) (NP DR CHAN)))))

;;; NOW DO THE SAME WITH TOP-DOWN RULES!!!!

;;; DAGUNIFY

(lib 'dagunify)

(get_value 'foo '((a b) (c d) (foo _x) (& _r)) '((_x apples)))
;;; (APPLES ((_X APPLES)))
(get_value 'foo '((a b) (& _x)) '((_x ((c d) (& _y))) (_y ((foo apples)(g h)(& _r)))))
;;; (APPLES ((_X ((C D) (& _Y))) (_Y ((FOO APPLES) (G H) (& _R)))))
(get_value 'foo '((a b) (c d) (baz _x) (& _y)) '((_x apples)))
;;; (_1 ((_Y ((FOO _1) (& _0))) (_X APPLES)))
(find_feature_value 'foo '((a b) (c d) (foo _x) (& _r)) '((_x apples)))
;;; APPLES
(find_feature_value 'foo '((a b) (& _x)) '((_x ((c d) (& _y))) (_y ((foo apples)(g h)(& _r)))))
;;; APPLES
(find_feature_value 'foo '((a b) (c d) (baz _x) (& _y)) '((_x apples)))
;;; ANY
(put_value_in '(baz now) '((a b) (& _x)) '((_x ((c d) (& _y))) (_y ((foo apples)(g h)(& _r)))))
;;; (((_R ((BAZ NOW) (& _4)))
;;;  (_X ((C D) (& _Y)))
;;;  (_Y ((FOO APPLES) (G H) (& _R))))
;;; ((A B) (C D) (FOO APPLES) (G H) (& _4)))
(put_value_in '(foo now) '((a b) (& _x)) '((_x ((c d) (& _y))) (_y ((foo _z)(g h)(& _r)))))
;;; (((_Z NOW) (_X ((C D) (& _Y))) (_Y ((FOO _Z) (G H) (& _R))))
;;;  ((A B) (C D) (G H) (& _R)))
(put_value_in '(foo now) '((a b) (& _x)) '((_x ((c d) (& _y))) (_y ((foo _z)(g h)(& _w)))))
;;; (((_Z NOW) (_X ((C D) (& _Y))) (_Y ((FOO _Z) (G H) (& _W))))
;;;  ((A B) (C D) (G H) (& _W)))
(unify '((a _x) (b B) (c _x) (& _r1)) '((b _y) (c C) (& _r2)))
;;; ((_R1 ((& _5))) (_X C) (_Y B) (_R2 ((A _X) (& _5))) ())
(unify '((a _x) (b B) (c _x) (& _r1)) '((b _y) (c C) (a D) (& _r2)))
;;; NIL
(unify '((a b)(c ((c1 12)(c2 14)(& _r1)))(& _r11)) '((c _x)(& _r2)))
;;; ((_R11 ((& _806))) (_X ((C1 12) (C2 14) (& _R1))) (_R2 ((A B) (& _806))) ())
(unify '((a a) (& _x)) '((b b) (c c) (& _y)))
;;; ((_X ((B B) (C C) (& _807))) (_Y ((A A) (& _807))) ())

;;; PATR

(lib 'lisp_patr)

(setq dag '((VP _x) (V _y) (X _z) (& _r)))
(setq subst
  (apply_condition '((VP slash) = (X slash)
     (V subcat) = (X cat)
     (V slash) = 0) dag empty_subst))
(simplify_features subst dag)
;;;((VP ((SLASH #:_112) (& #:_109)))
;;; (V ((SUBCAT #:_116) (SLASH 0) (& #:_117)))
;;; (X ((SLASH #:_112) (CAT #:_116) (& #:_115)))
;;; (& _R))

(let* (
   (goal '((cat S) (& _r)))
   (subst_rhs
     (lhs_match
       '((cat S) (& _r))
       '(Rule (S -> NP VP)
           (S slash) = (VP slash)
           (NP slash) = 0
           (S cat) = S
           (NP cat) = NP
           (VP cat) = VP))))
  (print (simplify_features (car subst_rhs) goal))
  (print (cadr subst_rhs))
  t)

;;;((CAT S) (SLASH #:_125) (& #:_122))
;;;(((SLASH 0) (CAT NP) (& #:_128))
;;; ((SLASH #:_125) (CAT VP) (& #:_130))) T

(lhs_match
   '((cat NP) (& _r))
   '(Word (Dr Chan)
      (slash) = 0
      (cat) = NP))

;;; (((_866 0) (_R ((SLASH _866) (& _865))) ())
;;; (DR CHAN))

(rhs_match
  '(
   ((cat NP) (& _r1))
   ((cat _unk) (slash NP) (& _r2))
   some
   other
   words)
  '(Rule (S -> NP VP)
   (S slash) = (VP slash)
   (NP slash) = 0
   (S cat) = S
   (NP cat) = NP
   (VP cat) = VP))
;;;(((_UNK VP)
;;;  (_910 S)
;;;  (_905 ((CAT _910) (& _909)))
;;;  (_908 0)
;;;  (_R1 ((SLASH _908) (& _907)))
;;;  (_906 NP)
;;;  (_903 ((SLASH _906) (& _905)))
;;;  ())
;;; (((SLASH NP) (CAT S) (& _909)) SOME OTHER WORDS))

(rhs_match
   '(Dr Chan died nurses)
   '(Word (Dr Chan)
      (slash) = 0
      (cat) = NP))

;;;(((_955 NP)
;;;  (_952 ((CAT _955) (& _954)))
;;;  (_953 0)
;;;  (_951 ((SLASH _953) (& _952)))
;;;  ())
;;; (((SLASH 0) (CAT NP) (& _954)) DIED NURSES))

;;; RANFGEN

(lib 'patrgram)
(lib 'randfgen)

(do () (nil) (print (g (make_dag '((cat) = S (slash) = 0)))))

;;;(MEDICENTER EMPLOYED DR CHAN)
;;;(NURSES SEEMED FIT)
;;;NIL
;;;(DR CHAN SEEMED FIT)
;;;(NURSES DIED)
;;;(PATIENTS DISAPPROVED OF PATIENTS)

;;; RANFTGEN

(lib 'patrgram)
(lib 'randftre)

(do () (nil) (print (g (make_dag '((cat) = S (slash) = 0)))))

;;;(S (NP NURSES) (VP (V APPROVED) (PP (P OF) (NP MEDICENTER))))
;;;(S (NP MEDICENTER) (VP (V DIED) (0)))
;;;NIL
;;;NIL
;;;(S (NP NURSES)
;;;   ((S / NP)
;;;    (NP MEDICENTER)
;;;    ((VP / NP)
;;;     (V THOUGHT)
;;;     ((S / NP)
;;;      (NP DR CHAN)
;;;      ((VP / NP) (V DISAPPROVED) ((PP / NP) (P OF) ((NP / NP))))))))

;;; FBUREC, PATRGRAM, AIRLINES, DISAMBIG

(lib 'fburecog)
(lib 'patrgram)

(recognize '(Dr Chan approved of Medicenter))
;;;(S)

(lib 'airlines)
(recognize '(every airline took over a hotel chain))
;;;(((QUANTIFIER ALL)
;;;  (VARIABLE #:_234)
;;;  (RESTRICTION ((PREDICATE AIRLINE) (ARG0 #:_234) (& #:_26)))
;;;  (BODY ((QUANTIFIER EXISTS)
;;;         (VARIABLE #:_204)
;;;         (RESTRICTION ((PREDICATE HOTEL_CHAIN) (ARG0 #:_204) (& #:_157)))
;;;         (BODY ((PREDICATE TOOK_OVER) (ARG0 #:_234) (ARG1 #:_204) (& #:_79)))
;;;         (& #:_207)))
;;;  (& #:_52)))

(lib 'disambig)
(recognize '(get a screwdriver with a narrow blade))    ;;; just produces VP
/*
(VP)
*/
(recognize '(get a screwdriver with your left hand))    ;;; just produces VP
/*
(VP)
*/

;;; FCHART, SUBSUMES, PATRGRAM, AIRLINES, DISAMBIG

(lib 'fchart)
(lib 'patrgram)
(chart_parse (make_dag '((cat) = S)) '(Medicenter died))
;;; ((S (NP MEDICENTER) (VP (V DIED) (0))))
(chart_parse (make_dag '((cat) = S)) '(Medicenter Dr Chan approved of))
;;;((S (NP MEDICENTER)
;;;   ((S / NP)
;;;    (NP DR CHAN)
;;;    ((VP / NP)
;;;     (V APPROVED)
;;;     ((PP / NP)
;;;      (P OF)
;;;      ((NP / NP)))))))

(lib 'airlines)
(chart_parse (make_dag '((cat) = S)) '(every airline took over a hotel chain))
/*
(((QUANTIFIER ALL)
  (& ((VARIABLE #:_30586)
      (& ((RESTRICTION ((PREDICATE AIRLINE) (ARG0 #:_30586) (& #:_30587)))
          (& ((BODY ((QUANTIFIER EXISTS)
                     (& ((VARIABLE #:_30588)
                         (& ((RESTRICTION
                              ((PREDICATE HOTEL_CHAIN) (ARG0 #:_30588) (& #:_30589)))
                             (& ((BODY ((PREDICATE TOOK_OVER)
                                        (ARG0 #:_30586)
                                        (ARG1 #:_30588)
                                        (& #:_30590)))
                                 (& #:_30591)))))))))
              (& #:_30592)))))))))
*/
(lib 'disambig)
(chart_parse (make_dag '((cat) = VP)) '(get a screwdriver with a narrow blade))
/*
((VP (TV GET)
     (NP (NP (DET A) (NB (N SCREWDRIVER)))
         (PP (P WITH) (NP (DET A) (NB (ADJ NARROW) (NB (N BLADE))))))))
*/

(chart_parse (make_dag '((cat) = VP)) '(get a screwdriver with your left hand))
/*
((VP (VP (TV GET) (NP (DET A) (NB (N SCREWDRIVER))))
            (PP (P WITH) (NP (DET YOUR) (NB (ADJ LEFT) (NB (N HAND)))))))
*/

;;; ** SUBST, DAGUNIFY, PATR, LEX

(lib 'lexicon)
(setq subst (apply_condition '(syn_tV) '_x empty_subst))
(simplify_features subst '_x)
;;;((SYN ((CAT V)
;;;       (ARG0 ((CAT NP) (CASE NOM) (& #:_50138)))
;;;       (ARG1 ((CAT NP) (CASE ACC) (& #:_50144)))
;;;       (& #:_50140)))
;;; (& #:_50130))

(apply_wfc example_wfc '_x (lookup_lex 'love))
;;;((MOR ((FORM ((STEM LOVE) (SUFFIX S) (& _10997))) (& _11041)))
;;; (SYN ((CAT V)
;;;       (ARG0 ((CAT NP) (CASE NOM) (PER 3) (NUM SING) (& _11047)))
;;;       (ARG1 ((CAT NP) (CASE ACC) (& _11037)))
;;;       (TENSE PRES)
;;;       (& _11049)))
;;; (SEM LOVE2A)
;;; (& _11051))

;;; ** TERMUNIFY, TERMSUBSUMES, TERMIFY, FCHART

(lib 'fchart)
(lib 'patrgram)
(lib 'termify)

(setq dag_patterns
 '(
   (? cat ? slash ? arg1 ? empty)
))

(setq rules
  (mapcar
    #'(lambda (x)
      (let*
        ((dag (newvar))
         (subst_dags (lhs_match dag x))
         (dag1 (apply_subst (car subst_dags) dag)))
        (mapcar #'termify_dag (cons dag1 (cadr subst_dags)))))
    rules))
(setq lexical_rules
  (mapcar
    #'(lambda (x)
      (let*
        ((dag (newvar))
         (subst_dags (lhs_match dag x))
         (dag1 (apply_subst (car subst_dags) dag)))
        (mapcar #'termify_dag (cons dag1 (cadr subst_dags)))))
    lexical_rules))

;;; access to rules in term form

(defun lhs_match (dag rule)
  (let* (
     (lhs (car rule))
     (rhs (cdr rule))
     (subst (termunify dag lhs)))
    (if subst
      (list subst (apply_subst subst rhs))
      '(() ()) )))

(defun rhs_match (dags rule)
  (let* (
     (lhs (car rule))
     (rhs (cdr rule))
     (needed (length rhs)))
    (if (< (length dags) needed)
      '(() ())
      (let ((subst (rhs_match_list rhs dags empty_subst)))
        (if subst
          (list
            subst
            (cons
              lhs
              (nthcdr needed dags)))
          '(() ()) )))))

(defun rhs_match_list (rhs dags subst) ; subst already applied
  (if (null rhs)
    subst
    (let ((newsubst (termunify (car rhs) (car dags))))
      (if newsubst
        (rhs_match_list
          (apply_subst newsubst (cdr rhs))
          (apply_subst newsubst (cdr dags))
          (compose_substs newsubst subst))
        nil))))

(defun unify (t1 t2)
   (termunify t1 t2))
(defun subsumes (t1 t2)
   (termsubsumes t1 t2))

(defun category (c subst)
  (let
    ((c1 (apply_subst subst c)))
    (cond
      ((equal (cadr c1) 0) (car c1))
      (t (list (car c1) '/ (cadr c1))))))

(defun tree (c trees)
   (cons c trees))

(chart_parse '(S _a _b _c) '(Medicenter died))
;;; ((S (NP MEDICENTER) (VP (V DIED) (0))))
(chart_parse '(S _a _b _c) '(Medicenter Dr Chan approved of))
;;;((S (NP MEDICENTER)
;;;   ((S / NP)
;;;    (NP DR CHAN)
;;;    ((VP / NP)
;;;     (V APPROVED)
;;;     ((PP / NP)
;;;      (P OF)
;;;      ((NP / NP)))))))

;;; CHAPTER 10
;;; TERMUNIFY, TERMSUBSUMES, TERMIFY, FCHART

(lib 'fchart)
(lib 'presupp)
(lib 'termify)

;;; modified rules, because of TERMIFY limitation

(setq rules
'(
(Rule (IMP -> V NP PP)
   (IMP cat) = IMP
   (V cat) = V
   (NP cat) = NP
   (PP cat) = PP
   (V p) = (PP p)
   (V arg1) = (NP referent)
   (V arg2) = (PP arg1)
   (IMP sem) = (V sem)
   (IMP pre connective) = and
   (IMP pre prop1) = (V pre)
   (IMP pre prop2 connective) = and
   (IMP pre prop2 prop1) = (NP pre)
   (IMP pre prop2 prop2) = (PP np_pre))
(Rule (NP -> DET N)
   (NP cat) = NP
   (DET cat) = DET
   (N cat) = N
   (NP pre) = (N pre)
   (NP referent) = (N referent))
(Rule (NP1 -> NP2 PP)
   (NP1 cat) = NP
   (NP2 cat) = NP
   (PP cat) = PP
   (NP1 referent) = (NP2 referent)
   (NP1 referent) = (PP arg0)
   (NP1 pre connective) = and
   (NP1 pre prop1) = (NP2 pre)
   (NP1 pre prop2 connective) = and
   (NP1 pre prop2 prop1) = (PP p_pre)
   (NP1 pre prop2 prop2) = (PP np_pre))
(Rule (PP -> P NP)
   (PP cat) = PP
   (P cat) = P
   (NP cat) = NP
   (PP arg0) = (P arg0)
   (PP arg1) = (NP referent)
   (PP arg1) = (P arg1)
   (PP p) = (P p)
   (PP np_pre) = (NP pre)
   (PP p_pre) = (P pre))
))

(setq dag_patterns
'(
 (predicate ? arg0 ? arg1 ? arg2)
 (connective prop1 prop2)
 ((cat IMP) ? sem ? pre)
 ((cat V) ? subcat ? p ? arg1 ? arg2 ? sem ? pre)
 ((cat PP) ? p ? arg0 ? arg1 ? p_pre ? np_pre)
 ((cat NP) ? pre ? referent)
 ((cat N) ? pre ? referent)
 ((cat P) ? p ? pre ? arg0 ? arg1)
 ((cat DET))
))

(setq rules
  (mapcar
    #'(lambda (x)
      (let*
        ((dag (newvar))
         (subst_dags (lhs_match dag x))
         (dag1 (apply_subst (car subst_dags) dag)))
        (mapcar #'termify_dag (cons dag1 (cadr subst_dags)))))
    rules))
(setq lexical_rules
  (mapcar
    #'(lambda (x)
      (let*
        ((dag (newvar))
         (subst_dags (lhs_match dag x))
         (dag1 (apply_subst (car subst_dags) dag)))
        (mapcar #'termify_dag (cons dag1 (cadr subst_dags)))))
    lexical_rules))

;;; access to rules in term form

(defun lhs_match (dag rule)
  (let* (
     (lhs (car rule))
     (rhs (cdr rule))
     (subst (termunify dag lhs)))
    (if subst
      (list subst (apply_subst subst rhs))
      '(() ()) )))

(defun rhs_match (dags rule)
  (let* (
     (lhs (car rule))
     (rhs (cdr rule))
     (needed (length rhs)))
    (if (< (length dags) needed)
      '(() ())
      (let ((subst (rhs_match_list rhs dags empty_subst)))
        (if subst
          (list
            subst
            (cons
              lhs
              (nthcdr needed dags)))
          '(() ()) )))))

(defun rhs_match_list (rhs dags subst) ; subst already applied
  (if (null rhs)
    subst
    (let ((newsubst (termunify (car rhs) (car dags))))
      (if newsubst
        (rhs_match_list
          (apply_subst newsubst (cdr rhs))
          (apply_subst newsubst (cdr dags))
          (compose_substs newsubst subst))
        nil))))

(defun unify (t1 t2)
   (termunify t1 t2))
(defun subsumes (t1 t2)
   (termsubsumes t1 t2))

(defun category (c subst)
(apply_subst subst (car c)))

(defun tree (c trees)
   (cons c trees))

(chart_parse '(IMP _x _y) '(put the screw on the box in it))
/*
((IMP (V PUT)
      (NP (DET THE) (N SCREW))
      (PP (P ON) (NP (NP (DET THE) (N BOX)) (PP (P IN) (NP IT)))))
 (IMP (V PUT)
      (NP (DET THE) (N SCREW))
      (PP (P ON) (NP (NP (DET THE) (N BOX)) (PP (P IN) (NP IT)))))
 (IMP (V PUT)
      (NP (NP (DET THE) (N SCREW)) (PP (P ON) (NP (DET THE) (N BOX))))
      (PP (P IN) (NP IT)))
 (IMP (V PUT)
      (NP (NP (DET THE) (N SCREW)) (PP (P ON) (NP (DET THE) (N BOX))))
      (PP (P IN) (NP IT))))
Duplication is due to a general PP search being spawned AFTER the more
specific search for an "on" PP. So is OK, I suppose
*/


;;; ** DBQ, COUNTRIES

(lib 'dbq)
(lib 'europe)

(query '(border france italy))
** ((()))
(query '(all _x (border _x italy) (printout _x)))
AUSTRIA
SWITZERLAND
FRANCE ((()))
(query '(all _x (country _x _pop _cap)
   (exists _y (border _x _y) (true))))
** ((()))
(query '(all _x (border _x italy) (border _x france)))
** NIL
(query '(all _x (and (border _x italy)
   (not (border _x france))) (printout _x)))
FRANCE
AUSTRIA ((()))
(query '(all _x (and (country _y _z _x)(border _y france))
   (printout _x)))
MADRID
ANDORRA
BRUSSELS
LUXEMBOURG
BONN
BERNE
ROME ((()))

;;; **FORINFER, BACKINFER

(lib 'bckinfer)

(setq infrules
'(
  ((s _s1 _s3) (np _s1 _s2) (vp _s2 _s3))
  ((vp (_v _x) _x) (verb _v))
  ((vp (_v _s1) _s2) (verb _v) (np _s1 _s2))
  ((np (_pn _x) _x) (pname _pn))
  ((pname mayumi))
  ((pname wieslyk))
  ((verb pleased))
))

(defun test ()
  (loop
    (let ((goal (read)))
      (dolist (subst (back_infer goal))
        (print (apply_subst subst goal))))))
(test)
(s _x _y)

(S (WIESLYK (PLEASED _Y)) _Y)
(S (WIESLYK (PLEASED (MAYUMI _Y))) _Y)
(S (WIESLYK (PLEASED (WIESLYK _Y))) _Y)
(S (MAYUMI (PLEASED _Y)) _Y)
(S (MAYUMI (PLEASED (MAYUMI _Y))) _Y)
(S (MAYUMI (PLEASED (WIESLYK _Y))) _Y) T

(lib 'forinfer)
(setq infrules
'(
  ((s _s1 _s3) (np _s1 _s2) (vp _s2 _s3))
  ((vp (_v _x) _x) (verb _v))
  ((vp (_v _s1) _s2) (verb _v) (np _s1 _s2))
  ((np (_pn _x) _x) (pname _pn))
))

(defun test ()
  (loop (for_infer (read))))

(test)
(verb hit)
Adding (VERB HIT)
Adding (VP (HIT _X) _X)
(pname mary)
Adding (PNAME MARY)
Adding (NP (MARY _X) _X)
Adding (S (MARY (HIT _S3)) _S3)
(pname jane)
Adding (PNAME JANE)
Adding (NP (JANE _X) _X)
Adding (S (JANE (HIT _S3)) _S3)

;;; **

(lib 'forinfer)
(lib 'families)

(defun test ()
  (loop (for_infer (read))))

(test)
(wife marg chris)
Adding (WIFE MARG CHRIS)
Adding (FEMALE MARG)
Adding (HUSBAND CHRIS MARG)
Adding (MALE CHRIS)
Adding (MARRIED CHRIS MARG)
Adding (MARRIED MARG CHRIS)
(son myth chris)
Adding (SON MYTH CHRIS)
Adding (CHILD MYTH CHRIS)
Adding (CHILD MYTH MARG)
Adding (PARENT MARG MYTH)
Adding (MOTHER MARG MYTH)
Adding (PARENT CHRIS MYTH)
Adding (FATHER CHRIS MYTH)
Adding (MALE MYTH)
Adding (SON MYTH MARG)
(female fm)
Adding (FEMALE FM)
(parent chris fm)
Adding (PARENT CHRIS FM)
Adding (CHILD FM CHRIS)
Adding (CHILD FM MARG)
Adding (DAUGHTER FM MARG)
Adding (DAUGHTER FM CHRIS)
Adding (FATHER CHRIS FM)
Adding (PARENT MARG FM)
Adding (MOTHER MARG FM)

;;; ** INHERITS

(lib 'inherits)

(get_attr 'kim 'associate_member)
YES
(get_attr 'kim 'sex)
MALE
(get_attr 'beryl 'sex)
FEMALE

;;; ** PRESUPP, BACKINFER

(lib 'presupp)
(lib 'termify)
(setq dag_patterns '(
 (predicate arg0 arg1)
 (predicate arg1 arg2)
 (predicate arg0)
 (predicate arg1)
 (predicate)
 (connective prop1 prop2)
 ((cat VP) ? sem ? pre)
 ((cat V) ? subcat ? p ? arg1 ? arg2 ? sem ? pre)
 ((cat PP) ? p ? arg0 ? arg1 ? p_pre ? np_pre)
 ((cat NP) ? pre ? referent)
 ((cat N) ? pre ? referent)
 ((cat P) ? p ? pre ? arg0 ? arg1)
 ((cat DET))
))
(lib 'bckinfer)
(lib 'fburecog)

(defun test (sentence)
  (mapc
    #'(lambda (parse)
      (print (list 'parse parse))
      (let ((presupps (car parse))(action (cadr parse)))
        (mapc
          #'(lambda (subst)
            (print (apply_subst subst (termify_dag action))))
          (back_infer (termify_dag presupps)))))
    (recognize sentence)) t)

(test '(put it in the box))
/*
(PUT_IN SCREW1 BOX1)
(PUT_IN SCREW2 BOX1)
(PUT_IN WASHER1 BOX1) T
*/
(test '(put it in the hole))
/*
(PUT_IN WASHER1 HOLE1)
(PUT_IN SCREW2 HOLE1) T
*/
(test '(put the washer on the screw in the box))
/*
** (PUT_IN WASHER1 BOX1)       ;;; (EVENTUALLY)
*/
(test '(put the screw in the hole))
/*
** (PUT_IN SCREW2 HOLE1)
*/

;;; ** PREDICT, BACKINFER

(lib 'predict)   ;;; includes inference rules
(progn
(mapc
  #'(lambda (x)
    (print x)
    (predict x))
  '(
   ((town mu))
   ((spire _x))
   ((famous (spire_of (church_of mu))))
   ((see people _x)))) t)
/*
((TOWN MU))
(ASSERTIONS NOT PREDICTED BUT UNAMBIGUOUS)
((SPIRE _X))
(ASSERTIONS UNIQUELY PREDICTED)
((SPIRE (SPIRE_OF (CHURCH_OF MU))))
((FAMOUS (SPIRE_OF (CHURCH_OF MU))))
(ASSERTIONS NOT PREDICTED BUT UNAMBIGUOUS)
((SEE PEOPLE _X))
(ASSERTIONS UNIQUELY PREDICTED)
((SEE PEOPLE (SPIRE_OF (CHURCH_OF MU)))) T
*/

;;; ** SCRIPTS

(lib 'scripts)  ;;; comes with example scripts
(script_match '((goes Jan Smiths)(tries_on _x hat45)(wears _y _z)))
/*
((HAT_BUY JAN HAT45 _ASSISTANT SMITHS)
 (GOES JAN SMITHS)
 (TRIES_ON JAN HAT45)
 (BUYS JAN HAT45 _ASSISTANT)
 (DELIVERS _ASSISTANT HAT45 JAN)
 (WEARS JAN HAT45))
*/

;;; PLAN

(lib 'plan)

(defun test (n)
  (setq infrules (car (example n)))
  (plan 'sue (cadr (example n))))

(defun example (n)
  (cond
	((eq n 1)
	 '(
	  ;; fig 2
	  ( ((channel sue alan))
       ((at alan inside))
       ((at sue inside))
	   )
	  ((at alan outside))
	  ))
	((eq n 2)
	 ;; fig 2
     (error "Do example 1 with request commented out" ()))
	((eq n 3)
	 ;; fig 5
	 '(
	  (
	   ((channel sue ann))
	   ((channel ann sue))
	   ((knows_ref ann combination))
	   )
	  ((knows_ref sue combination))
	  ))
	((eq n 4)
	 ;; fig 6 done as fig 7
	 '(
	  (
	   ((channel sue alan))
	   ((channel alan sue))
	   ((channel alan ann))
	   ((channel ann alan))
	   ((knows_ref ann combination))
	   )
	  ((knows_ref sue combination))
	  ))))

(test 1)

(TRYING DEPTH 0)
(TRYING DEPTH 1)
(TRYING DEPTH 2)
(TRYING DEPTH 3) ((REQUEST SUE ALAN (MOVE ALAN INSIDE OUTSIDE))
 (CAUSE_TO_WANT SUE ALAN (MOVE ALAN INSIDE OUTSIDE))
 (MOVE ALAN INSIDE OUTSIDE))

(setq request (car operators))
(setq operators (cdr operators))
(test 1) ;;; ie TEST 2

(TRYING DEPTH 0)
(TRYING DEPTH 1)
(TRYING DEPTH 2)
(TRYING DEPTH 3)
(TRYING DEPTH 4) ((INFORM SUE ALAN (WANT SUE (MOVE ALAN INSIDE OUTSIDE)))
 (CONVINCE SUE ALAN (WANT SUE (MOVE ALAN INSIDE OUTSIDE)))
 (CAUSE_TO_WANT SUE ALAN (MOVE ALAN INSIDE OUTSIDE))
 (MOVE ALAN INSIDE OUTSIDE))

(setq operators (cons request operators))
(test 3)

(TRYING DEPTH 0)
(TRYING DEPTH 1)
(TRYING DEPTH 2)
(TRYING DEPTH 3)
(TRYING DEPTH 4) ((REQUEST SUE ANN (INFORM_REF ANN SUE COMBINATION))
 (CAUSE_TO_WANT SUE ANN (INFORM_REF ANN SUE COMBINATION))
 (INFORM_REF ANN SUE COMBINATION)
 (CONVINCE_REF ANN SUE COMBINATION))

(test 4)

(TRYING DEPTH 0)
(TRYING DEPTH 1)
(TRYING DEPTH 2)
(TRYING DEPTH 3)
(TRYING DEPTH 4)
(TRYING DEPTH 5)
(TRYING DEPTH 6)
(TRYING DEPTH 7)
(TRYING DEPTH 8) ((REQUEST SUE ALAN (REQUEST ALAN ANN (INFORM_REF ANN ALAN COMBINATION)))
 (CAUSE_TO_WANT SUE ALAN (REQUEST ALAN ANN (INFORM_REF ANN ALAN COMBINATION)))
 (REQUEST ALAN ANN (INFORM_REF ANN ALAN COMBINATION))
 (CAUSE_TO_WANT ALAN ANN (INFORM_REF ANN ALAN COMBINATION))
 (INFORM_REF ANN ALAN COMBINATION)
 (CONVINCE_REF ANN ALAN COMBINATION)
 (INFORM_REF ALAN SUE COMBINATION)
 (CONVINCE_REF ALAN SUE COMBINATION))
