;;
 ; Common plot routines for Common LISP Compatibility -- for
 ; Harlequin/Xanalys LispWorks version 4.x
 ;
 ; Externally callable functions:
 ;
 ; init-plot( )         ;; creates a graphics window
 ; plot-fill-rect(x y xsize ysize value)    ;; fills a rectangle with a gray-scale value
 ; plot-size-rect(x y xsize ysize value)    ;; plots a rectangle value pixels wide
 ; clear-plot( )            ;; clears the graphics window
 ; pen-width (nibs)         ;; sets the pen drawing width
 ; plot-frame-rect(x y xsize ysize) ;; plots a frame rectangle
 ; plot-line(x1 y1 x2 y2)       ;; plots a line between two points
 ; show-plot( )         ;; shows graphics window
 ; plot-string(x y str)     ;; plots a string at position (x y)
 ; plot-string-bold(x y str)        ;; plots a bold string at position (x y)
 ; plot-string-italic(x y str)      ;; plots a italic string at position (x y)
 ; plot-mouse-down( )       ;; returns position of mouse click
 ;;

;;----------------------------------------------------------------------------
;;
;; plotlib4.lsp - a plotlib for Watson's book examples.
;;
;; Note: this library was closely modeled after the example program:
;;       examples/capi/graphics/pinboard-test.lisp
;; Because the example code was used, the following copyright
;; applies to this verion of plotlib:
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--98 Harlequin Group plc. All rights reserved.
;;----------------------------------------------------------------------------


;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST)

(in-package "CL-USER")

;;----------------------------------------------------------------------------
;; The callbacks for LispWorks CAPI graphics toolkit:
;;----------------------------------------------------------------------------

;; some global data to indicate if the mouse is currently
;; down:

(defun xy nil)

(defun press-button-1 (capi:interface x y)
  (setq xy (list x y)))

(defun release-button-1 (capi:interface x y)
  (setq xy nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Define the plotlib required functions:
;;

;;
 ; Initializes a standard plot window:
 ;;

(setq xx nil)
(setq normalfont nil)
(setq boldfont nil)
(setq normalfont nil)

(defun init-plot (&optional (title "Test Plot Window") (xSize 390) (ySize 430) )
  (setq xx
        (capi:contain
	 (make-instance 'capi:output-pane
			:title title
			:best-height xSize
			:best-width ySize
                        :input-model '(((:button-1 :press)   press-button-1)
				       ((:button-1 :release) release-button-1)))
         :best-height ySize
         :best-width xSize))
  (setq normalfont
        (gp:make-font-description
         :family "times" :size 12 :weight :medium
         :slant :roman))
  (setq boldfont
        (gp:make-font-description
         :family "times" :size 12 :weight :bold
         :slant :roman))
  (setq normalfont
        (gp:make-font-description
         :family "times" :size 12 :weight :medium
         :slant :italic)))



;;  (capi:display xx))

(setq xy nil)

(defun press-button-1 (capi:interface x y)
  (setq xy (list x y)))

(defun release-button-1 (capi:interface x y)
  (setq xy nil))

;;
 ; Fills in a rectangle with one of five gray-scale values:
 ;;

(setq color0 (color:make-rgb 1.0s0 1.0s0 1.0s0))
(setq color1 (color:make-rgb 0.8s0 0.8s0 0.8s0))
(setq color2 (color:make-rgb 0.6s0 0.6s0 0.6s0))
(setq color3 (color:make-rgb 0.4s0 0.4s0 0.4s0))
(setq color4 (color:make-rgb 0.2s0 0.2s0 0.2s0))

(defun plot-fill-rect (x y xsize ysize pattern &aux (c color0)) ;; 0 <= pattern <= 4
  (if (< pattern 1)
      (setq c color0)
    (if (< pattern 2)
	(setq c color1)
      (if (< pattern 3)
	  (setq c color2)
	(if (< pattern 4)
	    (setq c color3)
	  (setq c color4)))))
  (gp:draw-rectangle xx x y xsize ysize :foreground c :filled t))

;;
 ; Makes a black rectangle of size proportional to val. This is an alternative
 ; to using function plot-fill-rect for showing graphically the value of a number.
 ;;

(defun plot-size-rect (x y xsize ysize val)
  (if (> val xsize) (setq val xsize))
  (if (> val ysize) (setq val ysize))
  (if (< val 1) (setq val 1))
  (plot-fill-rect x y xsize ysize 0)
  (plot-fill-rect x y val val 4))

;;
 ; Clears (erases) the plot window:
 ;;

(defun clear-plot ()
  (plot-fill-rect 0 0 600 600 0))

;;
 ; Sets the drawing size for the pen:
 ;;

(defun pen-width (nibs)
  (gp:set-graphics-state xx :thickness nibs))

;;
 ; Frames a rectangle of size (xsize ysize) at position (x y):
 ;;

(defun plot-frame-rect (x y xsize ysize)
  (gp:draw-rectangle xx x y xsize ysize :filled nil :foreground color4))

;;
 ; Draws a line between two points:
 ;;

(defun plot-line (x1 y1 x2 y2)
  (gp:draw-line xx x1 y1 x2 y2))

;;
 ; Shows plot window if it is obscured:
 ;;

(defun show-plot ()
)

;;
 ; Plots a string at position (x y):
 ;;

(defun plot-string (x y str &optional (size 10))
 ;; (gp:set-graphics-state xx :font (gp:find-best-font normalfont))
  (gp:draw-string xx str x y))

;;
 ; Plots a string in bold font at position (x y):
 ;;

(defun plot-string-bold (x y str &optional (size 12))
 ;; (gp:set-graphics-state xx :font boldfont)
  (gp:draw-string xx str x y))

;;
 ; Plots a string in italic font at position (x y):
 ;;

(defun plot-string-italic (x y str)
;;  (gp:set-graphics-state xx :font italicfont)
  (gp:draw-string xx str x y))

;;
 ; Tests for a mouse down event (returns nil if the mouse button is
 ; not being held down when this function is called;  returns a list
 ; of the x and y screen coordinates relative to the plot window
 ; origin if the mouse button is being held down while this
 ; function is being called):
 ;;

(defun plot-mouse-down ()
  xy
)

;;
 ; A simple test program:
 ;;

(defun test ()
  (init-plot) ;; this line is not in the book
  (show-plot)
  (clear-plot)
  (dotimes (i 6)
    (plot-fill-rect
     (* i 9)
     (* i 9)
     8 8
     i)
    (plot-frame-rect (* i 9) (* i 9) 8 8))
  (dotimes (i 50)
    (plot-size-rect
     (+ 160 (random 200)) (random 100) (random 20) (random 20) (random 5)))
  (dotimes (i 4)
    (plot-string (* i 10) (+ 150 (* i 22)) "Mark's plot utilities..."))
  (plot-string-bold 20 260 "This is a test... of BOLD")
  (plot-string-italic 20 280 "This is a test... of ITALIC"))
