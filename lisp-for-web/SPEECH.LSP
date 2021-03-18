;;
  ; Speech Recognition Software.  Requires Delta Rule module and Plotting Module
 ;
 ; Copyright 1990 by Mark Watson
 ;;

;;
 ; Utility for finding word breaks in raw speech data:  function word-breaks
 ; is called with an array of data values between 0 and 255.  Raw data values of
 ; 128 indicate that at the time the data value was sampled, there was no sound
 ; amplitude.  Sound amplitude values are averaged over 50 raw data values:
 ;;

(defun word-breaks (data)
  (defun off-mean (start-index)
    (let ((retVal 0))
      (dotimes (i 50)
        (setq retVal (+ retVal
                        (abs (- (aref data (+ start-index i)) 128)))))
      (/ retVal 50)))
  (let ((len (length data))
         (word-boundaries)
         start stop  (inside-word? nil))
    (dotimes (nn (truncate (/ (- len 50) 10)))
      (let ((n (* nn 10)))
        (if inside-word?
          (if (<= (off-mean n) 3)
            (setq inside-word? nil
                  stop n
                  word-boundaries (cons (list start stop) word-boundaries)))
          (if (>= (off-mean n) 11)
            (setq inside-word? t
                  start (- n 50))))))
    (reverse word-boundaries)))
;;
 ;  Plot a highly downsampled plot of raw data with word breaks: function
 ;  raw-plot has two required arguments.  The first is an array of raw speech
 ;  data.  The second argument is a list of word break boundaries.  Raw data
 ;  is plotted on a compressed horizontal axis producing a scatter plot:
 ;;

(defun raw-plot (data word-boundaries)
  (let* ((len (length data))
          (num-words (length word-boundaries))
          (plot-width 400)
          retVal ; used to return the spectral features
          (data-points-per-pixel (truncate (/ len plot-width))))
    (init-plot "Raw Speech Data: 'Yes' and 'No'" 430 292)
    (dotimes (i len)
      (let ((x (truncate (/ i data-points-per-pixel)))
             (y (+ (aref data i) 5)))
        (plot-line x y x y)))
    (dotimes (i num-words)
      (let ((start-of-word
             (truncate (/ (car (nth i word-boundaries))
                          data-points-per-pixel)))
            (end-of-word
             (truncate (/ (cadr (nth i word-boundaries))
                          data-points-per-pixel))))
        (let ((spectra
               (spectral-features
                data
                (car (nth i word-boundaries))
                (cadr (nth i word-boundaries)))))
          (setq retVal (cons spectra retVal))
          (plot-spectra spectra start-of-word 285))
        (plot-line start-of-word 5 start-of-word 260)
        (plot-line start-of-word 260 end-of-word 260)
        (plot-line end-of-word 260 end-of-word 5)
        (plot-line end-of-word 5 start-of-word 5)))
    retVal))

;;
 ;  Process raw data: function spectra calls function word-breaks to calculate
 ;  the starting position of each word in the raw times series data.  Then
 ;  function raw-plot is called to calculate the spectra and produce data plots:
 ;;

(defun spectra (data-set)
  (raw-plot data-set (word-breaks data-set)))

;;
 ;  Extract spectral features: function spectral-features is called with an array
 ;  of raw data values and the starting and ending array indexes for a word.
 ;  A DFT is calculated to provide spectral energy levels (normalized
 ;  between 0.0 and 1.0):
 ;;

(defun spectral-features (data-set start stop) ; use a DFT to calculate the
  (defun clamp (value)                                   ; spectral features in desired
    (if (> value 0.5)                                         ; frequency bins.
      (setq value 0.5)
      (if (< value -0.5)
        (setq value -0.5)))
    (+ value 0.5))  ; range of returned data: (0.0 1.0)
  (let* ((len (- stop start))
          (sample-size 256)
          (nSamples 4)
          (nBins 7)
          retVal temp tempList
          (x-offset (truncate (/ (- len sample-size) nSamples))))
    (dotimes (ns nSamples)
      (setq tempList nil)
      (dotimes (nb nBins)
        (let ((k (* nb 40)))
          (setq temp (complex 0 0))
          (dotimes (nn sample-size)  ; loop to calculate the DFT
            (let ((n (+ start (* nb 40) (truncate (/ (* ns sample-size) 2)))))
              (setq temp
                    (+ temp
                       (* (aref data-set n)
                          (complex
                           (cos (/ (* -6.28318 n k) sample-size))
                           (sin (/ (* -6.28318 n k) sample-size))))))))
          (setq temp
                (clamp
                 (-
                  (log
                   (+ (* (realpart temp) (realpart temp))
                      (* (imagpart temp) (imagpart temp))
                      0.0001))
                  20.5))))  ;; delta rule model now has inputs scaled from 0.0 --> 1.0
        (setq tempList (cons temp tempList)))
      (setq retVal (append tempList retVal)))
  (reverse retVal)))

;;
 ;  Function plot-spectra plots a spectra at a given x y position:
 ;;

(defun plot-spectra (spectra x y)  ; plot vertical bar chart of spectral features at x,y
  (dolist (sf spectra)
    (let ((height (truncate (* 18  sf))))
      (plot-fill-rect x (- y height) 3 height 100))
    (setq x (+ x 4))))

;;
 ;  Function train  trains a 3 layer backprop network.
 ;  This function has one optional argument;  if this argument is not
 ;  supplied, then the network is initialized.  This function assumes the
 ;  global variables spec1, spec2, spec3, spec4, spec5, and spec6 have been
 ;  defined using the function spectra.  The function DeltaLearn (defined
 ;  in the neural network software module in chapter 3) is called in the autoplot
 ;  mode so neural network state plots can be examined by holding down the
 ;  mouse button during training.
 ;;

(defun train (&optional (restart 'yes))
  (init-plot "Training Yes/No Speech recognition neural network" 500 290)
  (if
    (equal restart 'yes)
    (setq temp (newdeltanetwork '(28 5 2))))
  (dotimes (ii 10000)
    (print (list ".............." ii))
    (print
     (deltalearn
      temp
      (list
       (list (car spec1) '(1.0 0.0))
       (list (cadr spec1) '(0.0 1.0))
       (list (car spec2) '(1.0 0.0))
       (list (cadr spec2) '(0.0 1.0))
       (list (car spec3) '(1.0 0.0))
       (list (cadr spec3) '(0.0 1.0))
       (list (car spec4) '(1.0 0.0))
       (list (cadr spec4) '(0.0 1.0)))
      'yes)))) ; autoplot mode