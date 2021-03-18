;;
 ; File: May-model-closeup.lisp
 ;
 ; Generates a magnified bifurcation diagram magnifying the area
 ; where period doubling turns into chaos. 
 ;
 ; Copyright 1990 by Mark Watson
 ;;

(defun bifur ()
  (init-plot "Magnification of Bifurcation around Period Doubling/Chaotic area"
                 542 257)  ;; specify title and x,y size of plot window
  (clear-plot)
  (show-plot)
  (let ((x-axis)
        (lambda) (x 0.1) (population))
    (dotimes (y-axis 220)
      (setq lambda (* 4 (+ 0.85 (/ y-axis 1500.0))))     ;;; change the growth rate
      (dotimes (iter 198)
        (setq population (* lambda x (- 1 x)))
        (setq x-axis (truncate (* population 500.02)))
        (if (and  (> x-axis 0)   (< x-axis 501))
          (plot-line x-axis y-axis (+ x-axis 1) y-axis))
        (setq x population)))))