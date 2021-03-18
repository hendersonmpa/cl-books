<0> (defun foo (pre)
<0> (do* ((l pre (cdr l)) (e (car pre) (car l)))
<0> ((null l))
<0> (print (list e l))))
FOO
<0> (foo '(a b c d e))

(A (A B C D E))
(A (B C D E))
(B (C D E))
(C (D E))
(D (E)) NIL
<0> (defun foo (pre)
<0> (do* ((l pre (cdr l)) (e (car l) (car l)))
<0> ((null l))
<0> (print (list e l))))
FOO
<0> (foo '(a b c d e))

(A (A B C D E))
(B (B C D E))
(C (C D E))
(D (D E))
(E (E)) NIL
<0> (defun foo (pre)
<0> (do* ((l (cdr pre) (cdr l)) (e (car pre) (car l)))
<0> ((null l))
<0> (print (list e l))))
FOO
<0> (foo '(a b c d e))

(A (A B C D E))
(B (B C D E))
(C (C D E))
(D (D E))
(E (E)) NIL
<0> (defun foo (pre)
<0> (do* ((l (cdr pre) (cdr l)) (e (car pre) (car l)))
<0> ((null e))
<0> (print (list e l))))
FOO
<0> (foo '())

(A (B C D E))
(C (C D E))
(D (D E))
(E (E)) NIL
<0> (foo '())
NIL
<0> (defun foo (pre)
<0> (do ((l (cdr pre) (cdr l)) (e (car pre) (car l)))
<0> ((null e))
<0> (print (list e l))))
FOO
<0> (foo '(a b c d e))
NIL
<0> (foo '(a b c d e))

(A (B C D E))
(C (C D E))
(D (D E))
(E (E)) NIL
<0> (defun foo (pre)
<0> (do ((l (cdr pre) (cdr l)) (e (car pre) (car l)))
<0> ((null e))
<0> (print (list e l))))
FOO
<0> (foo '())

(A (B C D E))
(B (C D E))
(C (D E))
(D (E))
(E ()) NIL
<0> (foo '())
NIL
<0> ;;; LOADING "/Lisp/Code/predict.lsp"
T

((TOWN MU))
(ASSERTIONS NOT PREDICTED BUT UNAMBIGUOUS)
((SPIRE _X))
(ASSERTIONS UNIQUELY PREDICTED)
((SPIRE (SPIRE_OF (CHURCH_OF MU))))
((FAMOUS (SPIRE_OF (CHURCH_OF MU))))
(ASSERTIONS NOT PREDICTED BUT UNAMBIGUOUS)
((SEE PEOPLE _X))
(ASSERTIONS UNIQUELY PREDICTED)
((SEE PEOPLE (SPIRE_OF (CHURCH_OF MU)))) (((TOWN MU))
 ((SPIRE _X))
 ((FAMOUS (SPIRE_OF (CHURCH_OF MU))))
 ((SEE PEOPLE _X)))

((TOWN MU))
(ASSERTIONS UNIQUELY PREDICTED)
((TOWN MU))
((SPIRE _X))
(ASSERTIONS UNIQUELY PREDICTED)
((SPIRE (SPIRE_OF (CHURCH_OF MU))))
((FAMOUS (SPIRE_OF (CHURCH_OF MU))))
(ASSERTIONS UNIQUELY PREDICTED)
((FAMOUS (SPIRE_OF (CHURCH_OF MU))))
((SEE PEOPLE _X))
(ASSERTIONS UNIQUELY PREDICTED)
((SEE PEOPLE (SPIRE_OF (CHURCH_OF MU)))) T
;;; LOADING "/Lisp/Code/scripts.lsp"
;;; Warning: undefined variable SUBST
T
;;; LOADING "/Lisp/Code/scripts.lsp"
T
((HAT_BUY JAN HAT45 _ASSISTANT SMITHS)
 (GOES JAN SMITHS)
 (TRIES_ON JAN HAT45)
 (BUYS JAN HAT45 _ASSISTANT)
 (DELIVERS _ASSISTANT HAT45 JAN)
 (WEARS JAN HAT45))
<0> scripts
(((AUTO_BUY _CUSTOMER _AUTO1 _AUTO2 _DRIVER _GARAGE)
  (GOES _CUSTOMER _GARAGE)
  (TEST_DRIVES _CUSTOMER _AUTO1)
  (ORDERS _CUSTOMER _AUTO2 _DRIVER)
  (DELIVERS _DRIVER _AUTO2 _CUSTOMER)
  (DRIVES _CUSTOMER _AUTO2))
 ((HAT_BUY _CUSTOMER _HAT _ASSISTANT _STORE)
  (GOES _CUSTOMER _STORE)
  (TRIES_ON _CUSTOMER _HAT)
  (BUYS _CUSTOMER _HAT _ASSISTANT)
  (DELIVERS _ASSISTANT _HAT _CUSTOMER)
  (WEARS _CUSTOMER _HAT)))
<0> 
(script_match '((goes Jan Smiths)(delivers _x hat45 _zz)(wears _y _z)))
NIL
<0> (script_match '((goes Jan Smiths)(delivers _x hat45 _zz)(wears _y _z)))
((HAT_BUY JAN HAT45 _X SMITHS)
 (GOES JAN SMITHS)
 (TRIES_ON JAN HAT45)
 (BUYS JAN HAT45 _X)
 (DELIVERS _X HAT45 JAN)
 (WEARS JAN HAT45))
<0> 
