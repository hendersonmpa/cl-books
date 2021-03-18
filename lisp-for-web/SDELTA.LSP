;;
 ;        Simple Delta Rule Neural Network Simulator.
 ;        This model supports only input and out layers,
 ;        with no hidden layers as seen in figure 3.3.
 ;
 ;        Copyright 1990 by Mark Watson.
 ;;

;;
 ; In order to make this example simulator modular and
 ; reusable, we encapsulate all data for a 2-layer
 ; delta rule neural network in one LISP data value.
 ; The following function creates a new network given
 ; the desired number of input and output neurons.
 ;;

(proclaim '(special *learning-rate* temp))

(defun makeDelta2 (numInputs numOutputs &aux theNetwork)
  (setq
   theNetwork
   (list
    (list numInputs numOutputs)
    (make-array numInputs :element-type 'float)  ; Input[numInputs]
    (make-array numOutputs :element-type 'float) ; Output[numOutputs]
    (make-array (list numInputs numOutputs)  ; Weight[numInputs][numOutputs]
               :element-type 'float)
    (make-array numOutputs :element-type 'float) ; SumProduct[numOutputs]
    (make-array numOutputs :element-type 'float) ; Error[numOutputs]
    (make-array (list numInputs numOutputs)
               :element-type 'float)))  ; DeltaWeight[numInputs][numOutputs]
  ;;
   ; Initialize the network connection strength weight values to random values:
   ;;
  (dotimes (in numInputs)
    (dotimes (out numOutputs)
      (setf (aref (Weight theNetwork) in out) (- (* (random 1000) 0.001) 0.5))))
  theNetwork)

;;
 ; Functions to return network components from the list structure
 ; defining our 2-layer delta rule networks:
 ;;
(defun NetWorkSize (theNetwork) (car theNetwork))
(defun Input (theNetwork) (cadr theNetwork))
(defun Weight (theNetwork) (cadddr theNetwork))
(defun Output (theNetwork) (caddr theNetwork))
(defun SumProduct (theNetwork) (cadddr (cdr theNetwork)))
(defun aError (theNetwork) (cadr (cddddr theNetwork)))
(defun DeltaWeight (theNetwork) (caddr (cddddr theNetwork)))

(setq *Learning-Rate* 0.6) ;; rate of connection-strength modification

;;
 ; Add a small amout of random noise to a list of numbers:
 ;;

(defun addNoiseToListOfNumbers (l)
  (defun tweak (x)
    (+
     x
     (- (* (random 1000) 0.0001) 0.05)))
  (mapcar #'tweak l))

;;
 ;  Perform training on our neural network data object  using
 ; the list of lists of training values supplied in the second argument:
 ;;

(defun Train2 (theNetwork trainingValues
               &aux (numInputs (car (NetWorkSize theNetwork)))
                    (numOutputs (cadr (NetWorkSize theNetwork)))
                    (Inputs (Input theNetwork))
                    (Outputs (Output theNetwork))
                    (Weights (Weight theNetwork))
                    (SumProducts (SumProduct theNetwork))
                    (Errors (aError theNetwork))
                    (DeltaWeights (DeltaWeight theNetwork))
                    (RMSerror 0.0))
  ;;
   ; Start out by zeroing the DeltaWeight array. DeltaWeight will be
   ; used to accumulate the delta weight changes for each connection
   ; strength weight over all supplied training values;  the Weight
   ; array is updated after one pass through all supplied training
   ; values (this is the optimum methodology. Modifying the weights
   ; after each individual training input/output example would require
   ; far more training time).
   ;;
  (dotimes (in numInputs)
    (dotimes (out numOutputs)
      (setf (aref DeltaWeights in out) 0.0)))
  ;;
   ; Loop over each input/output training example:
   ;;
  (dolist (inputOutputPair trainingValues)
    (let ((inputList (car inputOutputPair))
          (targetOutput (cadr inputOutputPair))
          tempError)

       ;;
        ; Add some random noise to the training inputs:
        ;;
      (setq inputList (addNoiseToListOfNumbers inputList))

       ;;
        ;  Recall2  propagates these input values
        ; through the network and calculate the output activation values:
        ;;
      (Recall2 theNetwork inputList)

       ;;
        ; Calculate an error for each output neuron scaled
        ; (using the derivative of the Sigmoid function)
        ; by the magnitude of the Sumproducts:
        ;;
      (dotimes (out numOutputs)
        (setq tempError  (- (nth out targetOutput) (aref Outputs out)))
        (setf
         (aref Errors out)
         (*
          tempError
          (SigmoidP (aref SumProducts out))))
        (setq RMSerror (+ RMSerror (* tempError tempError))))

       ;;
        ; Save the delta connection-strength contribution due
        ; to this input/output training pair:
        ;;
      (dotimes (in numInputs)
        (dotimes (out numOutputs)
          (setf
           (aref DeltaWeights in out)
           (+
            (aref DeltaWeights in out)
            (*
             *Learning-Rate*
             (aref Errors out)
             (aref Inputs in))))))))

  ;;
   ; After processing all input/output training examples, modify
   ; the connection strength weights:
   ;;
  (dotimes (in numInputs)
    (dotimes (out numOutputs)
      (setf
       (aref Weights in out)
       (+
        (aref Weights in out)
        (aref DeltaWeights in out)))))
  ;;
   ; Return the current RMS error for this training set:
   ;;
  RMSerror)

;;
 ; Recall2 propagates an input pattern through the network
 ; and returns an array of output layer neuron activation values
 ;;

(defun Recall2 (theNetwork inputList
                &aux (numInputs (car (NetWorkSize theNetwork)))
                     (numOutputs (cadr (NetWorkSize theNetwork)))
                     (Inputs (Input theNetwork))
                     (Outputs (Output theNetwork))
                     (Weights (Weight theNetwork))
                     (SumProducts (SumProduct theNetwork)))
  ;;
   ; Copy the input values to the input neurons:
   ;;
  (dotimes (in numInputs)
    (setf (aref Inputs in) (nth in inputList)))
  ;;
   ; Calculate the SumProducts array for the output neurons:
   ;;
  (dotimes (out numOutputs)
    (setf (aref SumProducts out) 0.0) ;; zero accumulator
    (dotimes (in numInputs)
      (setf
       (aref SumProducts out)
       (+
        (aref SumProducts out)
        (*
         (aref Inputs in)
         (aref Weights in out))))))

  ;;
   ; Use the Sigmoid function to convert the SumProducts array to
   ; the array Outputs (output activation energy):
   ;;
  (dotimes (out numOutputs)
    (setf
     (aref Outputs out)
     (Sigmoid (aref SumProducts out))))
  Outputs)

;;
 ; Sigmoid and derivative of Sigmoid functions:
 ;;

(defun Sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun SigmoidP (x)
  (let ((temp (Sigmoid x)))
    (* temp (- 1.0 temp))))

;;
 ;THROW AWAY TEST CODE:
 ;  (specify 2 input and 2 output neurons as seen in figure 3.3)
 ;;
(setq temp (makeDelta2 2 2)) 

;;
 ; Train the network and print total output error every 10 trining steps:
 ;;
(let (rms)
  (dotimes (n 51)
    (setq rms
          (Train2 temp
                  '(((1 0) (0 1))  ((0 1 0) (0 1)))))
    (if (equal (mod n 10) 0)
      (progn
        (princ "Current RMS error: ")  ;; This error signnal should decrease
        (princ rms)    (terpri)))))           ;; during training.
       