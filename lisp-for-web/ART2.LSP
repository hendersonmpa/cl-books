;;
 ;        Adaptive Resonance 2 Model in Common Lisp.
 ;        Copyright 1990 by Mark Watson.
 ;
 ;        Based on the Adaptive Resonance Theory
 ;        by Gail Carpenter and Stephen Grossberg.
 ;;

;;
 ; Define a separate name space package for this software module:
 ;;

;;(make-package :art2)   ;; optional
;;(export '(ART2 ART2-postprocess ART2-init)  :user)  ;; optional

;(special nInputs w x v r u q p temp resetVal y reset resetCounter nCategories temp2 wUp wDown)

(defun ART2-init (numInputs numOutputs &optional trainingPatterns)
  ; Check for specified training patterns:
  (if trainingPatterns
    ; Make sure the number of input neurons agrees with
    ; the size of the training patterns:
    (if (equal (length (car trainingPatterns)) numInputs)
      (setq testPatterns trainingPatterns)
      (print
       (list
        "ERROR: bad input to ART2-init. numInputs should have been"
        (length (car trainingPatterns)))))
    ; No specified training patterns: use the default set
    ; defined in this package:
    (if (not (equal (length (car testPatterns)) numInputs))
      (print
       (list
        "ERROR: bad input to ART2-init. numInputs should have been"
        (length (car testPatterns))))
      ; Specified number of input neurons agrees with
      ; the size of the default training patterns defined
      ; in this package; proceed with defining network data:
      (progn
        ; Define the network size:
        (setq nInputs numInputs)
        (setq nOutputs numOutputs)
        ; Array storage allocation:
        (setq input (make-array (list nInputs)))
        (setq w (make-array (list nInputs)))
        (setq x (make-array (list nInputs)))
        (setq v (make-array (list nInputs)))
        (setq r (make-array (list nInputs)))
        (setq u (make-array (list nInputs)))
        (setq q (make-array (list nInputs)))
        (setq p (make-array (list nInputs)))
        (setq temp (make-array (list nInputs)))
        (setq resetVal (make-array (list 1)))
        (setq y (make-array (list nOutputs)))
        (setq reset (make-array (list nOutputs)))
        (setq resetCounter (make-array (list nOutputs)))
        (setq nCategories (make-array (list nOutputs)))
        (setq temp2 (make-array (list nOutputs)))
        (setq wUp (make-array (list nInputs nOutputs)))
        (setq wDown (make-array (list nOutputs nInputs)))
        ; Global variable to remember input patterns and
        ; their associated output category code for plotting
        ; by function ART2-Postprocess:
        (setq *learned-categories* nil)))))

; Neuron size for plots:

(setq PEsize 18)
(setq PEsizem1 (- PEsize 1))
(setq PEsizep1 (+ PEsize 1))

; Model constants:

(setq a 0.5)
(setq b 0.2)
(setq c -1.0)
(setq d 0.4)
(setq e 0.04)
(setq theta 0.3) ;; 1 / (sqrt(nInputs))
(setq vigilance 0.94)
(setq alpha 1.0)

(setq resetThreshold 0.05)

(setq upLR 0.12)
(setq downLR 0.12)

 ; Floating point  random numbers:

(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

(defun findLargestOutput (&aux (maxIndex 0) (mVal (aref y 0)))
  (dotimes (j nOutputs)
    (if (and
         (> (aref y j) mVal)
         (not (aref reset j)))
      (setq mVal (aref y j)
            maxIndex j)))
  maxIndex)

; The following function returns d if (aref y index) is the
; largest value in array y AND (aref y index) has not been reset:

(defun g (index &aux j mVal (maxIndex (findLargestOutput)))
  (if (and
       (equal index maxIndex)
       (not (aref reset maxIndex))
       (> (aref y maxIndex) resetThreshold))
    d
    0.0))

; Threshold function:

(defun sigmoid (x)
  (if (> x theta)
    x
    0.0))

; L2 Norm of a vector:

(defun L2NORM (v vLength &aux (sum 0.0) i j)
  (dotimes (i vLength)
    (setq sum (+ sum (* (aref v i) (aref v i)))))
  (+ (sqrt sum) 0.001))

; Update F1 STM arrays:

(defun F1STMcycle (&aux i j sum norm max1 max2)
  ; Calculate p from u input and backwards feed back:
  (dotimes (i nInputs)
    (setq sum 0.0)
    (dotimes (j nOutputs)
      (setq sum (+ sum (* (g j) (aref wDown j i)))))
    (setf (aref p i) (+ (aref u i) sum)))
  ; Update q using eq. 5
  (setq norm (+ (L2NORM p nInputs) e))
  (dotimes (i nInputs)
    (setf (aref q i) (/ (aref p i) norm)))
  ; Update u using eq. 6:
  (setq norm (L2NORM v nInputs))
  (dotimes (i nInputs)
    (setf (aref u i) (/ (aref v i) norm)))
  ; Update v using eq. 7:
  (dotimes (i nInputs)
    (setf (aref v i) (sigmoid (+ (aref x i) (* b (sigmoid (aref q i)))))))
  ; Update w using eq. 8:
  (dotimes (i nInputs)
    (setf (aref w i) (* alpha (+ (aref input i) (* a (sigmoid (aref u i)))))))
  ; Update x using eq. 9:
  (setq norm (+ (L2NORM w nInputs) e))
  (dotimes (i nInputs)
    (setf (aref x i) (/ (aref w i) norm)))
  ; Calculate reset r from eq. 20:
  (setq max1 -1000.0 max2 -1000.0)
  (dotimes (j nInputs)
    (progn
      (if (< max1 (aref u j)) (setq max1 (aref u j)))
      (if (< max2 (aref p j)) (setq max2 (aref p j)))))
  (setq max1 (+ max1 0.001))
  (setq max2 (+ max2 0.001))
  (dotimes (i nInputs)
    (setf
     (aref r i)
     (- (/ (aref u i) max1) (/ (aref p i) max2)))))

; Update F2 STM storage:

(defun F2STMcycle (&aux i j sum)
  (dotimes (j nOutputs)
    (progn
      (setq sum 0.0)
      (dotimes (i nInputs)
        (setq sum (+ sum (* (aref p i) (aref wUp i j)))))
      (setf (aref y j) sum)
      (if (aref reset j) (setf (aref y j) -0.1)))))

; Update weights:

(defun updateWeights (&aux i (j (findLargestOutput)))
  (if (> (g j) 0.02)
    (dotimes (i nInputs)
      (setf
       (aref wDown j i)
       (+ (aref wDown j i)
          (*
           downLR
           d
           (- (aref p i) (aref wDown j i)))))
      (setf
       (aref wUp i j)
       (+
        (aref wUp i j)
        (*
         upLR
         d
         (- (aref p i) (aref wUp i j))))))))

; Competitive learning at slab F2:

(defun competitiveF2 (&aux i (jj (findLargestOutput)))
  (if (> (aref y jj) resetThreshold)
    (dotimes (i nOutputs)
      (if (not (equal i jj))
        (setf (aref y i) 0.0)))))

; Run one full cycle:

(defun OneCycle (&aux i j)
  (F1STMcycle)
  (testReset)
  (competitiveF2)
  (F2STMcycle)
  (updateWeights)
  (competitiveF2)
  (updateScreen))

; Check for an F2 reset condition:

(setq skipReset nil)

(defun testReset (&aux (res 0.0) (norm1 0.0) (norm2 0.0)
                       (n1 (+ (L2NORM p nInputs) e)) n2 temp)
  (if (and
       (> n1 0.2)
       (not skipReset))
    (if (> learningCycleCounter 1)
      (if (> (aref y (findLargestOutput)) 0.25)
        (setq res (* 3.0 (L2NORM r nInputs))))  ; was 3.0
      (setq skipReset nil)))
  (setf (aref resetVal 0) res)
  (plotActivations "reset flag" 190 220 resetVal 0.0 1.0)
  (if (> res (- 1.9 vigilance))  ;; 11/14/91 change
    (progn
      (print (list "Vigilance reset =" res "  Learning cycle ="
                   learningCycleCounter))
      (setq maxIndex (findLargestOutput))
      (setf (aref reset maxIndex) 1)
      (setf (aref resetCounter maxIndex) 80))
    (dotimes (i nOutputs)
      (setf (aref resetCounter i) (- (aref resetCounter i) 1))
      (if (< (aref resetCounter i) 0)
        (progn
          (if (aref reset i)  (setq skipReset t))
          (setf (aref reset i) nil)))))
  (setq skipReset nil)) ;; temporary

; Zero activations:

(defun zeroActivations ()
  (dotimes (i nInputs)
    (setf (aref w i) 0.0)
    (setf (aref x i) 0.0)
    (setf (aref v i) 0.0)
    (setf (aref r i) 0.0)
    (setf (aref u i) 0.0)
    (setf (aref q i) 0.0)
    (setf (aref p i) 0.0))
  (dotimes (i nOutputs)
    (setf (aref y i) 0)
    (setf (aref reset i) 0)
    (setf (aref resetCounter i) 0)))

; Set up a learning pattern in the input neurons:

(defun setPattern (pl &aux (len (length pl)))
  (if (not (equal len nInputs))
    (print (list "Error in setPattern input:" pl))
    (progn
      (setq learningCycleCounter 0)
      (zeroActivations)
      (dotimes (i len)
        (setf (aref input i) (+ (pop pl) (frandom -0.08 0.08)))))))

; Initialize the network:

(defun initNetwork ()
  (zeroActivations)
  (dotimes (j nOutputs)
    (progn
      (dotimes (i nInputs)
        (setf
         (aref wUp i j) (frandom 0.05 0.1)
         (aref wDown j i) (frandom 0.01 0.03)))
      (setf (aref nCategories j) 0))))

; Define some test patterns:

(setq testPatterns
      '(
        (0.0 0.4 0.9 0.4 0.0)
        (1.0 0.3 0.0 0.3 1.0)
        (0.75 0.5 0.02 0.05 0.07)
        (0.02 0.41 0.91 0.36 0.08)
        (1.1 0.12 0.0 0.3 1.0)
        (0.0 0.2 0.9 0.4 0.0)
        (0.93 0.31 0.0 0.78 1.0)
        (0.6 0.6 0.02 0.05 0.07)
        (0.79 0.54 0.02 0.08 0.07)
        (0.0 0.45 0.7 0.25 0.0)
        (1.0 0.3 0.0 0.3 0.65)
        (0.0 0.4 0.9 0.4 0.0)
        (0.0 0.5 0.75 0.5 0.0)
        (1.0 0.2 0.0 0.4 0.8)
        (0.82 0.46 0.02 0.09 0.01)))

; Cycle through all training patterns once:

(defun learnPatterns (num)
  (dolist (p testPatterns)
    (print (list "Inputs:" p))
    (setPattern p)
    (dotimes (i num)
      (setq learningCycleCounter (1+ learningCycleCounter))
      (OneCycle)
      (updateScreen))
    (setq *learned-categories*
          (cons (list p (findLargestOutput))
                *learned-categories*))))

 ; Graphics support:

(defun plotActivations (title x y data dmin dmax)
  (let ((size (array-dimension data 0)) (ypos 0) (xpos x))
    (plot-string-italic (- x 47) (+ y 7) title)
    (dotimes (i size)
      (if (< size 20)
        (setq ypos y xpos (+ x (* i PEsizep1)))
        (if (< i (/ size 2))
          (setq ypos (- y 7) xpos (+ x (* i PEsizep1)))
          (setq ypos (+ y 2)  xpos (+ x (* (- i (/ size 2)) PEsizep1)))))
      (plot-size-rect
       xpos ypos PEsizem1 PEsizem1
       (max
        1
        (truncate (* (/  (- (aref data i) dmin) (- dmax dmin)) PEsize))))
      (plot-frame-rect xpos ypos PEsize PEsize))))
  
(defun updateScreen ()
  (plotActivations "inputs" 50 10 input 0.0 1.0)
  (plotActivations "w" 50 30 w 0.0 1.0)
  (plotActivations "x" 50 50 x 0.0 1.0)
  (plotActivations "v" 50 70 v 0.0 1.0)
  (plotActivations "r" 50 90 r 0.0 1.0)
  (plotActivations "u" 50 110 u 0.0 1.0)
  (plotActivations "q" 50 130 q 0.0 1.0)
  (plotActivations "p" 50 150 p 0.0 1.0)
  (plotActivations "outputs" 50 200 y 0.0 1.0)
  (dotimes (i nOutputs)
    (if (aref reset i)
      (setf (aref temp2 i) 1.0)
      (setf (aref temp2 i) 0.0)))
  (plotActivations "reset" 50 220 temp2 0.0 1.0))

 ; Main test program:

(defun ART2 ()
  (ART2-init 5 4)
  (setq *learned-categories* nil)
  (init-plot "Adaptive Resonance Theory" 230 300)
  (initNetwork)
  (learnPatterns 50))

;;
 ;         Standalone program to plot the input
 ;         patterns and associated output category
 ;         codes produced by executing program ART2:
 ;;

(defun ART2-Postprocess (&aux (width 130) category-counts)

  (defun plot-input (x-org y-org values)
    (let ((delta-x (/ (- width 20) (length values))))
      (plot-line x-org y-org x-org (- y-org 20))
      (plot-line x-org y-org (+ x-org (- width 40)) y-org)
      (dotimes (n (- (length values) 1))
        (plot-line
         (+ x-org (* n delta-x))
         (- y-org (* 20 (nth n values)))
         (+ x-org (* (1+ n) delta-x))
         (- y-org (* 20 (nth (1+ n) values)))))))

  (setq category-counts (copy-tree '(0 0 0)))
  (init-plot "ART2: Category codes for input patterns" 382 270)
  (plot-string-italic 20 15 "Category 1")
  (plot-string-italic (+ 20 width) 15 "Category 2")
  (plot-string-italic (+ 20 width width) 15 "Category 3")
  (dolist (p *learned-categories*)
    (print (list "inputs:" (car p) "category:" (cadr p)))
    (let ((pattern (car p))
          (category-code (cadr p)))
      (plot-input
       (+ 20 (* category-code width)) ;; x coord of plot origin
       (+ 40 (* (nth category-code category-counts) 40)) ;; y coord
       pattern)
      (setf (nth category-code category-counts)
            (1+ (nth category-code category-counts))))))
