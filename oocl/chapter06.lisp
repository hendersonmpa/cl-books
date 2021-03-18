;; string search pg 184
(defun string-searchp (pattern text)
  (cond ((null (and (stringp pattern)
                    (stringp text)))
         (list 'error-non-strings pattern text))
        ((and (string= "" pattern)
              (string= "" text))
         t)
        ((or (string= "" pattern)
             (string= "" text))
         nil)
        (t
         (let ((index (position (char pattern 0) text)))
           (cond ((null index) nil)
                 ((string= pattern
                           (subseq text
                                   index (+ index (length pattern))))
                  t)
                 (t (string-searchp pattern
                                    (subseq text (1+ index)))))))))

;; spell checker pg 186

(defun tag-word (word)
  (setf (get word 'ISA-WORD) T)
  word)

(mapcar #'tag-word '(commuter computer computation computing compute computers))

(defun spell-match (word)
  (cond ((symbolp word) (setf word (string word))))
  (let ((word-length (length word)))
    (cond ((string= "" word) nil)
          ((sm-check word))
          ((sm-delection word word-length))
          ((sm-transposition word (- word-length 1)))
          ((sm-double word (- word-length 1)))
          ((sm-insertion word word-length))
          (t nil))))

;; Membership check
(defun sm-check (word)
  (cond ((stringp word) (setf word (intern word))))
  (cond ((get word 'ISA-WORD) word)
        (t nil)))

;; Delete letters
(defun sm-delection (word-string index)
  (cond ((zerop index) nil)
        ((let ((new-word
                (concatenate 'string
                             (subseq word-string 0 (- index 1))
                             (subseq word-string index))))
           (sm-check new-word)))
        (t (sm-deletion word-string (- index 1)))))

;; Two utility functions
(defun string-insert (word-string character index)
  "Insert a character into a string at given position"
  (cond ((or (not (stringp word-string))
             (not (characterp character))
             (not (integerp index))
             (< index 0)
             (> index (length word-string))
             (minusp index))
         'error-in-string-insert)
        (t (let ((s (copy-seq word-string)))
             (setf (elt s index) character)
             s))))

(defun string-swap (word-string location destination)
  "Swap a letter in the word at "
  (cond ((or (not (stringp word-string))
             (minusp location)
             (minusp destination)
             (>= location (length word-string))
             (>= destination (length word-string)))
         'error-in-string-swap)
        (t (let ((new-string (copy-seq word-string))
                 (temp-char (elt word-string location)))
             (setf (elt new-string location)
                   (elt new-string destination))
             (setf (elt new-string destination) temp-char)
             new-string))))


;; Transpose letters
(defun sm-transposition (word-string index)
  (cond ((zerop index) nil)
        ((sm-check (string-swap word-string index (- index 1))))
        (t (sm-transposition word-string (- index 1)))))

;; Insert double letters
(defun sm-double (word-string index)
  (cond ((minusp index) nil)
        ((sm-check (string-insert word-string
                                  (elt word-string index)
                                  index)))
        (t (sm-double word-string (- index 1)))))
;; Insertion check
(defun sm-insertion-check (word-string index new-char)
  (cond ((char> new-char #\Z) nil)
        ((sm-check (string-insert word-string new-char index)))
        (t (sm-insertion-check word-string index
                               (incr-char new-char)))))

(defun incr-char (character)
  (code-char (1+ (code-char character))))
