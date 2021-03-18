(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(let1 foo (+ 2 3)
  (princ "Lisp is awesome!")
  (* foo foo))

(macroexpand '(let1 foo (+ 2 3)
               (* foo foo)))

;; my list tail call optimized
(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))

;;; Buggy version
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

(split (progn (princ "Lisp Rocks!")
              '(2 3))
       (format t "This list can be split into ~a and ~a." head tail)
       (format t "This list cannot be split."))

(macroexpand '(split (progn (princ "Lisp Rocks!")
                            '(2 3))
               (format t "This list can be split into ~a and ~a." head tail)
               (format t "This list cannot be split.")))

(progn (princ "lisp Rocks")
       '(2 3))

(defmacro split2 (val yes no)
  `(let ((x ,val))
     (if x
         (let ((head (car x))
               (tail (cdr x)))
           ,yes)
         ,no)))

(split2 (progn (princ "Lisp Rocks!")'(2 3))
        (format t "This list can be split into ~a and ~a." head tail)
        (format t "This list cannot be split."))

(macroexpand '(split2 (progn (princ "Lisp Rocks!")
                             '(2 3))
               (format t "This list can be split into ~a and ~a." head tail)
               (format t "This list cannot be split.")))

(defmacro split3 (val yes no)
  (let ((g (gensym)))
    `(let ((,g ,val))
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))
(macroexpand '(split3 '(2 3)
               (+ x head)
               nil))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(pairs '(a b c d e f))

(defmacro recurse (vars &body body)
  (let ((p (pairs vars)))
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))
