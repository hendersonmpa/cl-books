;;
 ; Common LISP Hopfield Network for associative memory recall.
 ;               Copyright 1989 by Mark Watson
 ;
 ; The following function is passed a list
 ; of lists of training examples.  A Hopfield
 ; network is built in memory to recognize
 ; patterns similar to the training examples.
 ; This function returns a list containing the
 ; data defining a Hopfield neural network.
 ; All data for the network is encapsulated
 ; in the list returned by this function, so
 ; multiple Hopfield neural networks can be
 ; used in an application program.
 ;;

(proclaim '(special *num-inputs* *num-training-examples* *training-list*
                    *inputCells* *tempStorage* *HopfieldWeights*))

(defun Hopfield-Init (training-data
                      &aux temp *num-inputs* *num-training-examples*
                           *training-list* *inputCells* *tempStorage*
                           *HopfieldWeights*)

  ;;
   ; Allocate global arrays for data storage:
   ;;

  (setq *num-inputs* (length (car training-data)))
  (setq *num-training-examples* (length training-data))

  (setq *training-list* (make-array (list *num-training-examples* *num-inputs*)))
  (setq *inputCells* (make-array (list *num-inputs*)))
  (setq *tempStorage* (make-array (list *num-inputs*)))
  (setq *HopfieldWeights* (make-array (list *num-inputs* *num-inputs*)))

  ;;
   ;  Copy training data:
   ;;

  (dotimes (j *num-training-examples*)
    (dotimes (i *num-inputs*)
      (setf
       (aref *training-list* j i)
       (nth i (nth j training-data)))))
  ;;
   ; Utility to adjust data values to values of -1.0 or +1.0 only:
   ;;
  (defun adjustInput (value)  ;; this function is lexically scoped
    (if (< value 0.1)
      -1.0
      +1.0))
 
  ;;
   ;  Adjust training data to -1.0 and +1.0 values:
   ;;

  (dotimes (i *num-inputs*)
    (dotimes (n *num-training-examples*)
      (setf
       (aref *training-list* n i)
       (adjustInput (aref *training-list* n i)))))

  (dotimes (i *num-inputs*)
    (dotimes (j *num-inputs*)
      (setf (aref *HopfieldWeights* i j) 0)))

  ;;
   ; Calculate the autocorrelation weight matrix from the input test patterns:
   ;;

  (dotimes (j-1 (- *num-inputs* 1))
    (let ((j (+ j-1 1)))
      (dotimes (i j)
        (dotimes (s *num-training-examples*)
          (setq temp
                (truncate
                 (+
                  (*  ;; 2 if's truncate values to -1 or 1:
                   (adjustInput (aref *training-list* s i))
                   (adjustInput (aref *training-list* s j)))
                  (aref *HopfieldWeights* i j))))
          (setf (aref *HopfieldWeights* i j) temp)
          (setf (aref *HopfieldWeights* j i) temp)))))
  (dotimes (i *num-inputs*)
    (setf (aref *tempStorage* i) 0)
    (dotimes (j i)
      (setf (aref *tempStorage* i)
            (+ (aref *tempStorage* i) (aref *HopfieldWeights* i j)))))
  ;;
   ; Return the value of the Hopfield network data object:
   ;;

  (list
   *num-inputs* *num-training-examples* *training-list*
   *inputCells* *tempStorage* *HopfieldWeights*))


;;
 ; Iterate the network to let it settle in a stable pattern
 ; which will (we hope) be the original training pattern
 ; most closely resembling the noisy test pattern:
 ;;

(defun HopfieldNetRecall (aHopfieldNetwork numberOfIterations)

    ; Define a block of code in which all data components of our
    ; test Hopfield neural network are defined as lexically scoped:
    (let ((*num-inputs* (nth 0 aHopfieldNetwork))
           (*num-training-examples*  (nth 1 aHopfieldNetwork))
           (*training-list* (nth 2 aHopfieldNetwork))
           (*inputCells* (nth 3 aHopfieldNetwork))
           (*tempStorage* (nth 4 aHopfieldNetwork))
           (*HopfieldWeights* (nth 5 aHopfieldNetwork)))
   ;;
    ; Calculate a change in energy from old input
    ; values and the autocorrelation weight matrix:
    ;;
 
  (defun deltaEnergy (k y &aux (temp 0.0))  ;; this function is lexically scoped
    (dotimes (j *num-inputs*)
      (setq temp (+ temp (* (aref *HopfieldWeights* k j) (aref y j)))))
    (- (* 2.0 temp) (aref *tempStorage* k)))

  ;;
   ; Main code for HopfieldNetRecall:
   ;;

  (dotimes (ii numberOfIterations)
    (dotimes (i *num-inputs*)
      (setf (aref *inputCells* i)
            (if (> (deltaEnergy i *inputCells*) 0)
                 1
              0))))))



;;
 ;    Throwaway test code:  to run, execute: (test)
 ;
 ;   You can edit the following value plot-size to
 ;   change the size of the cells in the output graph:
 ;;

(setq plot-size 8)
(setq plot-size+1 (+ plot-size 1))

;;
 ; Main test program: execute (test) in your Lisp listener window
 ;;

(defun test (&aux aHopfieldNetwork)
  (init-plot "Hopfield pattern classifier" 280 160)
  (let ((tdata '(  ;; sample sine wave data with different periods:
                 (1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 1 1 0 0 0)
                 (0 1 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 1 0)
                 (0 0 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 1 1 0 1 1))))

    ; Set up network:
    (setq aHopfieldNetwork (Hopfield-Init tdata))

    ;;
     ; Define a block of code in which all data components of our
     ; test Hopfield neural network are defined as lexically scoped
     ; variables to make writing the test code easier:
     ;;

    (let ((*num-inputs* (nth 0 aHopfieldNetwork))
           (*num-training-examples*  (nth 1 aHopfieldNetwork))
           (*training-list* (nth 2 aHopfieldNetwork))
           (*inputCells* (nth 3 aHopfieldNetwork))
           (*tempStorage* (nth 4 aHopfieldNetwork))
           (*HopfieldWeights* (nth 5 aHopfieldNetwork)))
      
      ;;
       ; Define debug functions for plotting data values.  Note that
       ; these plot functions are all lexically scoped inside function test.
       ;;
      
      (defun plotExemplar (row &aux (dmin 0.0) (dmax 1.0) (x 20) (y 30))
        (let ((Xsize (array-dimension *training-list* 0))
              (YSize (array-dimension *training-list* 1)))
          (plot-string-italic (+ x 20) (- y 10) "Original Training Exemplar")
          (dotimes (j Ysize)
            (plot-fill-rect
             (+ x (* j plot-size+1)) y plot-size plot-size
             (truncate (* (/  (- (aref *training-list* row j) dmin) (- dmax dmin)) 5)))
            (plot-frame-rect (+ x (* j plot-size+1)) y plot-size plot-size))))
      
      
      (defun plot-*original-inputCells* (&aux (dmin 0.0) (dmax 1.0) (x 20) (y 70))
        (let ((Xsize (array-dimension *inputCells* 0)))
          (plot-string-italic (+ x 20) (- y 10) "Scrambled Inputs")
          (dotimes (j Xsize)
            (plot-fill-rect
             (+ x (* j plot-size+1)) y plot-size plot-size
             (truncate (* (/  (- (aref *inputCells* j) dmin) (- dmax dmin)) 5)))
            (plot-frame-rect (+ x (* j plot-size+1)) y plot-size plot-size))))
      
      (defun plot-*inputCells* (&aux (dmin 0.0) (dmax 1.0) (x 20) (y 110))
        (let ((Xsize (array-dimension *inputCells* 0)))
          (plot-string-italic (+ x 20) (- y 10) "Reconstructed Inputs")
          (dotimes (j Xsize)
            (plot-fill-rect
             (+ x (* j plot-size+1)) y plot-size plot-size
             (truncate (* (/  (- (aref *inputCells* j) dmin) (- dmax dmin)) 5)))
            (plot-frame-rect (+ x (* j plot-size+1)) y plot-size plot-size))))
      
      ;;
        ; Define a utility function to scramble training inputs
        ; for use in testing the Hopfield network.  This function is lexically scoped:
       ;;
      
      (defun modifyInput (arrSize arr)  ;; modify input array for testing
        (dotimes (i arrSize)
          (if (< (random 50) 5)
            (if (> (aref arr i) 0)
              (setf (aref arr i) -1)
              (setf (aref arr i) 1)))))
      
      ;;
        ; Test network on training data that is randomly modified:
       ;;

      (dotimes (iter 10) ;; user can break out early
        (dotimes (s *num-training-examples*)
          (dotimes (i *num-inputs*)
            (setf (aref *inputCells* i) (aref *training-list* s i)))
          (plotExemplar s)
          (modifyInput *num-inputs* *inputCells*)
          (plot-*original-inputCells*)
 
          ;;
            ; NOTE: usually you will want to call function HopfieldNetRecall
            ;                   with a second argument of 5 (to indicate 5 iterations
            ;                   to allow the network to relax into the correct state).
            ;                   However, here we call it with a second argument of 1
            ;                   in a loop so we can plot the network activation values
            ;                   in between relaxation cycles:
            ;;

          (dotimes (call-net 5) ;; iterate Hopfield net 5 times
            (HopfieldNetRecall aHopfieldNetwork 1)  ;; calling with 1 iteration
            (plot-*inputCells*))

          ; delay a while for viewing the activation energy plots:
          (dotimes (delay 3000000) nil))))))  ;; change delay for your system
