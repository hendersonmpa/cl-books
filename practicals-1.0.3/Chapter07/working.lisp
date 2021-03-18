(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))

(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) (print cur)) (print cur))

(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums) (print nums) )

(loop for i from 1 to 10 collecting i)

(loop for x across "the quick brown fox jumps over the lazy dog"
     counting (find x "aeiou"))
