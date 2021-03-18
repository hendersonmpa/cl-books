;;
 ; Capture hand-drawn numbers from 0 to 9 and train a neural network
 ; to recognize them.
 ;
 ;  Copyright 1990 by Mark Watson
 ;;

(setq *symbols* '("0" "1" "2" "3" "4" )) ;;    "5" "6" "7" "8" "9"))
(setq xScale 7)
(setq yScale 9)

(defun start ()
  (setq *H-network*
        (newdeltanetwork
         (list (* xScale yScale) 8 (length *symbols*))))
  (init-plot "Handwritten numbers")
  (setq *training-list* nil))

(defun capturePoints ()
  (defun differentPoints (p1 p2)
    (let ((xdif (- (car p1) (car p2)))
          (ydif (- (cadr p1) (cadr p2))))
      (> (+ (* xdif xdif) (* ydif ydif))  5)))
  (clear-plot)
  (show-plot)
  (let ((xmin 9999) (ymin 99999) (xmax -9999) (ymax -9999)
         retList picList pos (oldPos '(-100 -100)))
    (dotimes (i 40000)
      (setq pos (plot-mouse-down))
      (if pos
        (if (differentPoints pos oldPos)
          (let ((x (car pos)) (y (cadr pos)))
            (plot-fill-rect (- x 2) (- y 2) 5 5 100)
            (if (< x xmin) (setq xmin x)
                (if (> x xmax) (setq xmax x)))
            (if (< y ymin) (setq ymin y)
                (if (> y ymax) (setq ymax y)))           
            (setq retList
                  (cons pos retList)
                  oldPos pos)))))
    (if (< (abs (- xmin xmax)) 30) ;; special case for 1
      (setq xmin (- xmin 200)
            xmax (+ xmax 200)))
    (let ((xminB 4) (yminB 20)
           (xmaxB (+ 4 xScale)) (ymaxB (+ 20 yScale)))
      ;;
       ; Define a return picture list with the correct number of elements:
       ;;
       (dotimes (x xScale)
        (dotimes (y yScale)
          (setq picList (cons 0.2 picList))))
      (plot-line (- xminB 3) (- yminB 3) (+ xmaxB 3) (- yminB 3))
      (plot-line (+ xmaxB 3) (- yminB 3) (+ xmaxB 3) (+ ymaxB 3))
      (plot-line (+ xmaxB 3) (+ ymaxB 3) (- xminB 3) (+ ymaxB 3))
      (plot-line (- xminB 3) (+ ymaxB 3) (- xminB 3) (- yminB 3))
      (dolist (pos retList)
        (let ((x (car pos)) (y (cadr pos)))
          (let* ((xp (truncate (* 0.999 (* (/ (- x xmin) (- xmax xmin)) xScale))))
                  (yp (truncate (* 0.999 (* (/ (- y ymin) (- ymax ymin)) yScale))))
                  (index (+ xp (* yp xScale))))
            (setf (nth index picList) 0.8)
            (plot-line (+ 4 xp) (+ 20 yp) (+ 4 xp) (+ 20 yp))))))   
    (dotimes (delay 200000) nil)   ;; delay a bit..
    picList))
;;
 ;  Utility for recording training examples:
 ;;

(defun record ()
  (let (retVal (count 0) inputs)
    (dolist (sym *symbols*)
      (princ "Draw the character ")
      (princ sym)
      (princ " in the graphics window.")
      (terpri)
      (setq inputs (capturePoints))
      (setq *target-output* nil)
      (dotimes (ii (length *symbols*))
        (if (equal ii count)
          (setq *target-output* (cons  0.8 *target-output*))   ;; different than book example
          (setq *target-output* (cons  0.2 *target-output*)))) ;; different than book example
      (setq *target-output* (reverse *target-output*))
      (setq count (+ count 1))
      (setq retVal (cons (list inputs *target-output*) retVal)))
    (setq *training-list* (cons (reverse retVal) *training-list*))))

;;
 ;  Utility for training the network using the complete delta rule
 ;  module developed in chapter 3:
 ;;

(defun train (&optional (restart 'yes))
  (princ "Starting training session.  Hold down mouse for plots")
  (terpri)
  (setq defaultEidaList '(0.2 0.2 0.2 0.2))
  (setq defaultNoise 0.15)
  (if
    (equal restart 'yes)
    (setq temp (newdeltanetwork (list (* xScale yScale) 8 (length *symbols*)))))
  (dotimes (ii 500000)
    (if (equal ii 100)
      (setq defaultEidaList '(0.1 0.1 0.1 0.1)
            defaultNoise 0.1))
    (if (equal ii 500)
      (setq defaultEidaList '(0.05 0.05 0.05 0.05)
            defaultNoise 0.08))
    (if (equal ii 1500)
      (setq defaultEidaList '(0.03 0.03 0.03 0.03)
            defaultNoise 0.03))
    (princ "RMS error = ")
    (princ
     (deltalearn
      *H-network*
      (car *training-list*)
      'yes)) ; autoplot mode
    (terpri)))

;;
 ;  Utility for testing the network after training:
 ;;

(defun recall ()
  (let* ((pattern (capturePoints))
        (retVal (DeltaRecall *H-network* pattern)))
    (dotimes (i (length *symbols*))
      (if (> (nth i retVal) 0.4)
        (progn (princ "Symbol was ") (princ i) (terpri))))))