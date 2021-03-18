;;
 ; File: May-model.lisp
 ;
 ; Population growth model of Robert May: generates a bifurcation diagram
 ; showing large scale effect of the population growth rate.
 ; Note: the area to the upper right of the initial curve indicates
 ; extinction due to the rate of population growth being too small
 ; (the variable lambda).
 ;
 ;  Copyright 1990 by Mark Watson
 ;;

(defun bifur ()
  (init-plot "Global Population Growth Bifurcation Diagram" 502 230)
  (clear-plot)
  (show-plot)
  (plot-string 236 22 "Extinction")
  (plot-string 16 41 "Steady state")
  (plot-string 334 123 "Period doubled")
  (plot-string 190 215 "Chaos")
  (let ((x-axis)
        (lambda) (x 0.1) (population))
    (dotimes (y-axis 198)
      (setq lambda (* 4 (+ 0.20 (/ y-axis 250.0))))
      (dotimes (iter 198)
        (setq population (* lambda x (- 1 x)))
        (setq x-axis (truncate (* population 500.02)))
        (if (and  (> x-axis 0)  (< x-axis 501))
          (plot-line x-axis y-axis (+ x-axis 1) y-axis))
        (setq x population)))))
