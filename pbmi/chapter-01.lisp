;;; 1.1.2.3 Fasta files
(defconstant +aa-codes+
  (let ((valid-letters "ABCDEFGHIKLMNPQRSTVWY"))
    (append (map 'list #'identity valid-letters)
            (map 'list #'char-downcase valid-letters))))

;;; pg 52
(defun read-fasta (strm &optional (allowed-chars +aa-codes+)
                          (ac-parser #'sp-parser))
  (labels
      ((scan (accum)
         (let (( char (read-char strm nil :eof)))
           (cond ((eql char :eof) (reverse accum))
                 ((eql char #\>) (unread-char char strm)
                  (reverse accum))
                 ((member char allowed-chars)
                  (scan (cons (intern (string-upcase (string char)))
                              accum)))
                 (t (scan accum))))))
    ;;the ac-parser call will return nil of end of file is reached
    (let ((accession-number (funcall ac-parser strm)))
      (if (null accession-number)
          nil
          (list accession-number (scan nil))))))

(defun sp-parser (strm)
  (let ((ac (make-string 6))
        (ch (read-char strm nil :eof))) ;; get rid of the >
    (if (eql ch :eof) nil ;; at end of file!
        (progn
          (dotimes (i 6)
            (setq ch (read-char strm nil :eof))
            (if (eql ch :eof) ;; shouldn't happen
                (return-from sp-parser nil)
                (setf (aref ac i) ch)))
          (read-line strm nil :eof)
          ac))))
