; ; File: IFS.lisp
  ;
  ; Iterated function system for recreating images from IFS tables
  ;
  ; Copyright 1990 by Mark Watson
  ;;

;;
 ;  Data specification for two IFS designs: a Sunflower
 ;  and a Fern. Set one flag (only) from the following
 ;  list to a non-nil value to produce either the Sunflower
 ;  or Fern design:
 ;;

(setq SUNFLOWER nil)
(setq FERN T)

(if SUNFLOWER
  (setq title "IFS Example - Sunflower with Magnification"
        XSCALE 40
        YSCALE 26
        XORG 260
        YORG 180
        maxTable 4
        a '(-0.30  -0.30     0.02  0.00)
        b '(-0.60  -1.00     0.01  0.02)
        c '(1.10    1.0      0.00  0.00)
        d '(0.11    0.010   -0.2   0.00)
        e '(4.00    4.00     0.00  0.01)
        f '(0.00    0.00     0.00  0.02)
        p '(0.02    0.95     0.01  0.02)
        x1 1.2 y1 0.8 x2 1.8 y2 2.1 ;; define zoom box
        XZOOMORG -270 YZOOMORG 340
        zx1 (+ (* x1 XSCALE 6) XZOOMORG)
        zy1 (- YZOOMORG (* y1 YSCALE 6)) 
        zx2 (+ (* x2 XSCALE 6) XZOOMORG)
        zy2 (- YZOOMORG (* y2 YSCALE 6))))

(if FERN
  (setq  title "IFS Example - Fern with Magnification of Leaf"
         XSCALE 75
         YSCALE 21
         XORG 250
         YORG 260
         maxTable 4
         a '(0.85 -0.15 0.2 0.0)
         b '(0.04 0.28 -0.26 0.0)
         c '(-0.04 0.26 0.23 0.0)
         d '(0.85 0.24 0.22 0.16)
         e '(0.0 0.0 0.0 0.0)
         f '(1.6 0.44 1.6 0.0)
         p '(0.85 0.065 0.065 0.02)
        x1 -0.85 y1 6.1 x2 -0.6 y2 7.1 ;; define zoom box
        XZOOMORG 390 YZOOMORG 900
        zx1 (+ (* x1 XSCALE 6) XZOOMORG)
        zy1 (- YZOOMORG (* y1 YSCALE 6)) 
        zx2 (+ (* x2 XSCALE 6) XZOOMORG)
        zy2 (- YZOOMORG (* y2 YSCALE 6))))

(setq LIMIT 150000)  ;; limit on iterations

;;
 ; IFS program:
 ;;
(defun IFS ()
  (let ((pt 0.0)
        (probability-table '(0 0 0 0 0 0))
        x y pk newx newy)

    ;; Ppen a graphics window:
    (init-plot title 470 270)

    ;; Plot a small "capture" box to enlarge in the zoom box:
    (plot-line
     (+ (* x1 XSCALE) XORG) (- YORG (* y1 YSCALE))
     (+ (* x2 XSCALE) XORG) (- YORG (* y1 YSCALE)))
    (plot-line
     (+ (* x2 XSCALE) XORG) (- YORG (* y1 YSCALE))
     (+ (* x2 XSCALE) XORG) (- YORG (* y2 YSCALE)))
    (plot-line
     (+ (* x2 XSCALE) XORG) (- YORG (* y2 YSCALE))
     (+ (* x1 XSCALE) XORG) (- YORG (* y2 YSCALE)))
    (plot-line
     (+ (* x1 XSCALE) XORG) (- YORG (* y2 YSCALE))
     (+ (* x1 XSCALE) XORG) (- YORG (* y1 YSCALE)))

    ;; Plot the outline of the enlarged zoom box:
    (plot-line zx1 zy1 zx2 zy1)
    (plot-line zx2 zy1 zx2 zy2)
    (plot-line zx2 zy2 zx1 zy2)
    (plot-line zx1 zy2 zx1 zy1)

    ;; Scale the probabilities:
    (dotimes (i maxTable)
      (setq pt (+ pt (nth i p)))
      (setf (nth i probability-table) pt))

    ;; Set the initial position of a point:
    (setq x 0.0 y 0.0)

    ;; Main iteration loop:
    (dotimes (i LIMIT)
      (setq pk (random 1.01))
      (if (<= pk (nth 0 probability-table))
        (setq k 0)
        (if (<= pk (nth 1 probability-table))
          (setq k 1)
          (if (<= pk (nth 2 probability-table))
            (setq k 2)
            (setq k 3))))
      (setq newx (+
                  (* (nth k a) x)
                  (* (nth k b) y)
                  (nth k e)))
      (setq newy (+
                  (* (nth k c) x)
                  (* (nth k d) y)
                  (nth k f)))
      (setq x newx  y newy)

      ;; Throw way the first 20 points to wait for stabilization:
      (if (> i 19)
        (let ((ix (truncate (+ (* x XSCALE) XORG)))
              (iy (truncate (- YORG (* y YSCALE)))))
          ;; Plot only the first 30000 points for the entire
          ;; design to prevent over-saturation of points.
          ;; Note that we plot more points in the
          ;; enlarged zoom box since the points are
          ;; spread out on the screen by a factor of 6.
          ;; in each direction.
          (if (< i 30000)
            (plot-line ix iy ix iy))
          ;; Check to see if point (ix,iy) is inside the zoom box:
          (if (and
               (> x x1)
               (< x x2)
               (> y y1)
               (< y y2))
            (let ((ixx (truncate (+ (* x XSCALE 6) XZOOMORG)))
                  (iyy (truncate (- YZOOMORG (* y YSCALE 6)))))
              (plot-line ixx iyy ixx iyy))))))))
