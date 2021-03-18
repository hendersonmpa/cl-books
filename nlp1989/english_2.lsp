;;; ENGLISH_2 - RTN for a fragment of English

(defvar networks)

(setq networks
 '(
   (S
      (
        (Initial  (0))
        (Final    (2))
        (From 0 to 1 by NP)
        (From 1 to 2 by VP)
      )
   )

   (NP
      (
        (Initial  (0))
        (Final    (2))
        (From 0 to 1 by DET)
        (From 1 to 2 by N)
        (From 2 to 3 by WH)
        (From 3 to 2 by VP)
      )
   )

   (VP
      (
        (Initial  (0))
        (Final  (1 2))
        (From 0 to 1 by V)
        (From 1 to 2 by NP)
        (From 1 to 3 by that)
        (From 3 to 2 by S)
      )
   )
 ))

(setq abbreviations
 '(
   (N woman house table)
   (NP Mayumi Maria Washington)
   (DET a the that)
   (V sees hits sings lacks)
   (WH who which that)
  )
)
