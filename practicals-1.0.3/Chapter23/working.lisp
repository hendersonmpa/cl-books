(asdf:oos 'asdf:load-op :spam)

(defparameter *total-spams* 1000)
(defparameter *total-hams* 1000)
(defun spam-probability (spam-count ham-count)
  (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
         (ham-frequency (/ ham-count (max 1 *total-hams*))))
    (/ spam-frequency ( + spam-frequency ham-frequency))))

(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun push-list (list)
  (let ((new-list ()))
    (dolist (item list)
      (push (* 2 item) new-list)) new-list))

(dolist (x '(1 2 3 4 5))
  (print (* 2 x)))

(defparameter probs '( 0.02 0.01 0.3 0.2 0.2 0.01))

(log (reduce #'* probs))
(reduce #'+ probs :key #'log)
