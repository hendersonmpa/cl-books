;;
 ; File: Mandelbrot.lisp
 ;
 ; Calculate and plot the Mandelbrot set
 ; Copyright 1990 by Mark Watson
 ;;

;;
 ; Define the maximum number of gray-scale or color values
 ; that function plot-fill-rect uses as a pattern (or color) range:
 ; (Plot-fill-rect is defined in the graphics library in Chapter 2.)
 ;;
(setq MAX_COLORS 5)

;;
 ; Define the number of complex points to evaluate for Mandelbrot
 ; set membership: a rectangle of points num-x-cells by num-y-cells:
 ;;

(setq num-x-cells 90      num-y-cells 65)

;;
 ; Set the cell width and height for plotting a single point
 ; on the complex plane:
 ;;

(setq cell-width 2)
(setq delta-x-cell (/ 3.2 num-x-cells)     delta-y-cell (/ 3.2 num-y-cells))

;;
 ;  Function M  is called to calculate the Mandelbrot set
 ; for complex points around the complex number zero.
 ;;

(defun M (&optional (debug nil))
  (init-plot "Mandelbrot Plot"
             (+ 10 (* cell-width num-x-cells))
             (+ 25 (* cell-width num-y-cells)))
  (clear-plot)
  (show-plot)
  (let ((x 0.5) (y 0.5) (z (complex 0 0)))
    (dotimes (ix num-x-cells)
      (setq x (- (* ix delta-x-cell) 2.0))
      (dotimes (iy num-y-cells)
        (setq y (- (* iy delta-y-cell) 1.6))
        (setq z (complex x y))
        (let ((index nil))
          (dotimes (i 60)
            (setq z (+ (complex x y) (* z z)))
            (setq index i)
            (if (>  (+  (* (realpart z) (realpart z))  (* (imagpart z) (imagpart z))) 4)
              (setq i 1000)))
          (plot-fill-rect 
           (+ 10 (* ix cell-width))
           (+ 10 (* iy cell-width))
           cell-width cell-width (truncate (mod index MAX_COLORS))))))))