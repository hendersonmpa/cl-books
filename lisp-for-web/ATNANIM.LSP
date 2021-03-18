;;
 ; File: ATNanimation.LISP
 ;
 ;  To execute this sample program, load this file into your
 ;  Lisp workspace after loading the plotting library. Then
 ;  execute the function test. This example program plots  sample
 ;  ATN networks graphically and illustrates the execution of the
 ;  networks by hilighting each node in the network when the
 ;  incoming arc (if test) accepts the next word in the sentence.
 ;
 ;      Copyright 1989 by Mark Watson
 ;;


;;
 ; Word desciminators:
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

(setq *s1-np2?-thickness* 1)
(setq *done-np2?-thickness* 1)
(setq *start-np2?-thickness* 1)

(defun np? ()
  (let ((save *sentence*)
        (save-s1 *s1-np2?-thickness*)
        (save-done *done-np2?-thickness*)
        (save-start *start-np2?-thickness*))
    (draw-start-np? 1)
    (if
      (and
       (if (word? 'adj)
         (let ()
           (draw-s1-np? 1)
           (draw-start-np? -1))
         (let ()
           (draw-start-np? -1)
           nil))
       (if (np?)
         (let ()
           (draw-done1-np? 1)
           (draw-s1-np? -1)
           (ATN-delay)
           (draw-done1-np? -1))
         (let ()
           (draw-start-np? -1)
           (draw-s1-np? -1)
           (setq *sentence* save
                 *s1-np2?-thickness* save-s1
                 *done-np2?-thickness* save-done
                 *start-np2?-thickness* save-start)
           nil)))
      T
      (let ()
        (draw-start-np2? 1)
        (if (word? 'noun)
          (let ()
            (draw-done1-np2? 1)
            (draw-start-np2? -1)
            (ATN-delay)
            (draw-done1-np2? -1))
          (draw-start-np2? -1))))))

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
  (let ((save *sentence*)
        (save-s1 *s1-vp?-thickness*)
        (save-done *done-vp?-thickness*)
        (save-start *start-vp?-thickness*))
    (draw-start-vp? 1)
    (if (or
         (and
          (if (word? 'verb)
            (let ()
              (draw-s1-vp? 1)
              (draw-start-vp? -1)))
          (if (np?)
            (let ()
                (draw-done1-vp? 1)
                (draw-s1-vp? -1)
                (setq *s1-vp?-thickness* save-s1
                      *done-vp?-thickness* save-done
                      *start-vp?-thickness* save-start)
                (ATN-delay)
                (draw-done1-vp? -1)
                T)))
         (let ()
           (setq *sentence* save
                 *s1-vp?-thickness* save-s1
                 *done-vp?-thickness* save-done
                 *start-vp?-thickness* save-start)
           (if (word? 'verb)
             (let ()
               (draw-done1-vp? 1)
               (setq *s1-vp?-thickness* save-s1
                     *done-vp?-thickness* save-done
                     *start-vp?-thickness* save-start)
               (draw-start-vp? -1)
               (ATN-delay)
               (draw-done1-vp? -1)))))
      t
      (let ()
        (setq *sentence* save
              *s1-vp?-thickness* save-s1
              *done-vp?-thickness* save-done
              *start-vp?-thickness* save-start)
        nil))))

(defun sentence? ()
  (let ((save *sentence*))
    (draw-start 1)
    (if (or
         (and
          (if (np?)
            (let ()
              (draw-s1 1)
              (draw-start -1)))
          (if (vp?)
            (draw-s2 1)
            (let ()
              (draw-s1 -1)
              (draw-s2 1)))
          (if (prepp?)
              (draw-done1 1)
              (draw-s2 -1)))
         (let ()
           (setq *sentence* save)
           (and (np?)
                (vp?))))
      (let ()
        (draw-done1 1)
        (draw-s2 -1)
        (draw-done1 -1)
        (draw-s1 -1)
        T)
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
          '((noun isNoun?) (verb isVerb?)
            (prep isPrep?) (adj isAdj?)))))
    (princ "Processing word: ") (princ w) 
    (princ "  sentence: ") (princ *sentence*) (terpri)
    (if func
      (if (funcall (cadr func) w)
        (let ()
          (setq *sentence* (cdr *sentence*))
          t)
        nil)
      nil)))

;;
 ; Define test words:
 ;;
(setq *nouns* '(block dog I))

(setq *verbs* '(grab see))

(setq *adjs* '(blue green large red the))

(setq *preps* '(behind under))

(setq *subject* nil)

(setq *last-noun* nil)
      
;; turn on tracing:

(trace isNoun? isVerb? isAdj? isPrep? np? prepp? vp? sentence?)

;;
 ; Run the system:
 ;;
(setq nodeSizeX 40)
(setq nodeSizeY 20)

(defun plot-node (name x y thickness)
  (ATN-delay)
  ;; erase old node:
  (plot-fill-rect x y nodeSizeX nodeSizeY 0) 
  (plot-string (+ x 8) (+ y nodeSizeY -7) name)
  (if (< thickness 1) (setq thickness 1))
  (dotimes (n thickness)
    (let ((xmin (+ x n))
          (ymin (+ y n))
          (xsize (- nodeSizeX (* n 2)))
          (ysize (- nodeSizeY (* n 2))))
      (plot-frame-rect xmin ymin xsize ysize))))

(defun plot-arrow (x1 y1 x2 y2)
  (plot-line x1 y1 x2 y2)
  (plot-line x2 y2 (- x2 4) (- y2 4))
  (plot-line x2 y2 (- x2 4) (+ y2 4)))


;;
 ; Animation code for sentence? network:
 ;;
(setq *s1-thickness* 1)
(setq *s2-thickness* 1)
(setq *done-thickness* 1)
(setq *start-thickness* 1)

(defun draw-start (delta)
  (setq *start-thickness* (+ *start-thickness* delta))
  (plot-node "sent?" 35 20 *start-thickness*)
  T)

(defun draw-s1 (delta)
  (setq *s1-thickness* (+ *s1-thickness* delta))
  (plot-node 's1 85 20 *s1-thickness*)
  T)

(defun draw-s2 (delta)
  (setq *s2-thickness* (+ *s2-thickness* delta))
  (plot-node 's2 135 20 *s2-thickness*)
  T)

(defun draw-done1 (delta)
  (setq *done-thickness* (+ *done-thickness* delta))
  (plot-node 'done 185 20 *done-thickness*)
  T)

(defun draw-sentence ()
  (setq *s1-thickness* 1)
  (setq *s2-thickness* 1)
  (setq *done-thickness* 1)
  (setq *start-thickness* 1)
  (plot-arrow 46 27 85 27)
  (plot-arrow 100 27 135 27)
  (plot-arrow 150 27 185 27)
  (draw-start 0)
  (draw-s1 0)
  (draw-s2 0)
  (plot-string-italic 70 15 "np?")
  (plot-string-italic 120 15 "vp?")
  (plot-string-italic 165 15 "prepp?")
  (draw-done1 0))
  
;;
 ; Animation code for np? network number 1:
 ;;
(setq *s1-np?-thickness* 1)
(setq *done-np?-thickness* 1)
(setq *start-np?-thickness* 1)


(defun draw-start-np? (delta)
  (setq *start-np?-thickness* (+ *start-np?-thickness* delta))
  (plot-node "np?" 35 60 *start-np?-thickness*)
  T)

(defun draw-s1-np? (delta)
  (setq *s1-np?-thickness* (+ *s1-np?-thickness* delta))
  (plot-node 's1 85 60 *s1-np?-thickness*)
  T)

(defun draw-done1-np? (delta)
  (setq *done-np?-thickness* (+ *done-np?-thickness* delta))
  (plot-node 'done 135 60 *done-np?-thickness*)
  T)

(defun draw-np? ()
  (setq *s1-np?-thickness* 1)
  (setq *s2-np?-thickness* 1)
  (setq *done-np?-thickness* 1)
  (setq *start-np?-thickness* 1)
  (plot-arrow 46 67 85 67)
  (plot-arrow 100 67 135 67)
  (plot-string-italic 68 55 "adj?")
  (plot-string-italic 120 55 "np?")
  (draw-start-np? 0)
  (draw-s1-np? 0)
  (draw-done1-np? 0))

;;
 ; Animation code for np? network number 2:
 ;;
(setq *s1-np2?-thickness* 1)
(setq *done-np2?-thickness* 1)
(setq *start-np2?-thickness* 1)


(defun draw-start-np2? (delta)
  (setq *start-np2?-thickness* (+ *start-np2?-thickness* delta))
  (plot-node "np?" 35 100 *start-np2?-thickness*)
  T)

(defun draw-s1-np2? (delta)
  (setq *s1-np2?-thickness* (+ *s1-np2?-thickness* delta))
  (plot-node 's1 85 100 *s1-np2?-thickness*)
  T)

(defun draw-done1-np2? (delta)
  (setq *done-thickness* (+ *done-thickness* delta))
  (plot-node 'done 85 100 *done-np2?-thickness*)
  T)

(defun draw-np2? ()
  (setq *s1-np2?-thickness* 1)
  (setq *s2-np2?-thickness* 1)
  (setq *done-np2?-thickness* 1)
  (setq *start-np2?-thickness* 1)
  (plot-arrow 46 107 85 107)
  (plot-string-italic 65 95 "noun?")
  (draw-start-np2? 0)
  (draw-done1-np2? 0))

;;
 ; Animation code for vp? network:
 ;;
(setq *s1-vp?-thickness* 1)
(setq *done-vp?-thickness* 1)
(setq *start-vp?-thickness* 1)


(defun draw-start-vp? (delta)
  (setq *start-vp?-thickness* (+ *start-vp?-thickness* delta))
  (plot-node "vp?" 35 140 *start-vp?-thickness*)
  T)

(defun draw-s1-vp? (delta)
  (setq *s1-vp?-thickness* (+ *s1-vp?-thickness* delta))
  (plot-node 's1 85 140 *s1-vp?-thickness*)
  T)

(defun draw-done1-vp? (delta)
  (setq *done-vp?-thickness* (+ *done-vp?-thickness* delta))
  (plot-node 'done 135 140 *done-vp?-thickness*)
  T)

(defun draw-vp? ()
  (setq *s1-vp?-thickness* 1)
  (setq *s2-vp?-thickness* 1)
  (setq *done-vp?-thickness* 1)
  (setq *start-vp?-thickness* 1)
  (plot-arrow 46 147 85 147)
  (plot-arrow 100 147 135 147)
  (plot-string-italic 68 135 "verb?")
  (plot-string-italic 120 135 "np?")
  (draw-start-vp? 0)
  (draw-s1-vp? 0)
  (draw-done1-vp? 0))


;;
 ; Delay function to slow down the animation 
 ; (ADJUST FOR THE SPEED OF YOUR MACHINE):
 ;;
(setq *ATN-delay* 185000)
(defun ATN-delay ()
  (dotimes (delay *ATN-delay*) nil))

;;
 ; Initialize graphics:
 ;;
(defun init-diagrams ()
  (setq *ATN-delay* 0)
  (init-plot "Animated ATN" 290 200)
  (draw-sentence)
  (draw-np?)
  (draw-np2?)
  (draw-vp?)
  (setq *ATN-delay* 80000)) ; set delay for your machine !!

;;
 ;  MAIN test function:
 ;;
(defun test ()
  ;; Reset the test sentence:
  (setq *sentence* 
        '(I see the large red block under the large dog))
  (init-diagrams)
  (sentence?))