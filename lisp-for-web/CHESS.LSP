;;
 ;  Chess Program by Mark Watson
 ;
 ; Bug list: (does not handle en passant capture)
 ;
 ; Copyright 1978-1990 by Mark Watson
 ;;

(proclaim
 '(special
   *wking-moved* *wrook1-moved* *wrook2-moved* *bking-moved*
   *brook1-moved* *brook2-moved* *move-num* *board*
   *human-square-control* *computer-square-control*
   *index* *piece* *value* *debug* *old-board* *old-moves*
   *moves-for-current-piece*))

;;
 ; Define global variables:
 ;;
(setq *board* (make-array (list 120))) ;; board
(setq *computer-square-control* (make-array (list 120)))
(setq *human-square-control* (make-array (list 120)))
(setq *moves-for-current-piece* nil)
(setq *index* '(0 12 15 10 1 6 0 0 0 6)) ;; piece index into move table
(setq *piece* '(0 -1 1 10 -10 0 1 -1 10 -10 -9 -11 9 11 0 8 -8 12 -12
                        19 -19 21 -21 0 10 20 0 0 0 0 0)) ;; piece move table

;;
 ; Set value of the pieces: pawn, knight, bishop, rook, queen, and king:
 ;;
(setq *value* '(0 1 3 3 5 9 0 0 0 25))
;;
 ;  Turn on debug output:
 ;;
(setq *debug* t)

;;
 ; Remember last board state to allow replaying the last move
 ; when modifying the program:
 ;;
(setq *old-board* (make-array (list 120)) *old-moves* 0)

;;
 ;  Use constants to define piece values:
 ;;
(defconstant PAWN 1)
(defconstant KNIGHT 2)
(defconstant BISHOP 3)
(defconstant ROOK 4)
(defconstant QUEEN 5)
(defconstant KING 9)

;;
 ; Format of the board (e.g., square A1 is square 22 and
 ; square H8 is square 99 in the indexing scheme used
 ; for the board squares):
 ;;
;  Square index:                   Board with pieces:
;  -------------------                  -------------------------
;
;
;  92 93 94 95 96 97 98 99    BR   BN   BB    BQ    BK  BB     BN     BR
;  82 83 84 85 86 87 88 89    BP    BP   BP    BP    BP   BP     BP    BP
;  72 73 74 75 76 77 78 79      .       X      .       X      .      X         .        X
;  62 63 64 65 66 67 68 69    X      .         X      .        X      .        X      .  
;  52 53 54 55 56 57 58 59    .       X      .       X        .      X         .        X 
;  42 43 45 45 46 47 48 49    X      .         X      .        X      .        X      . 
;  32 33 34 35 36 37 38 39    WP  WP   WP   WP    WP   WP   WP  WP
;  22 23 24 25 26 27 28 29    WR  WN   WB   WQ   WK   WB   WN  WR

;; Note: Human (white) pieces are positive numbers and
;; computer (black pieces) are negative numbers.  For
;; example, if (aref *board* 55) equals -2, then square
;; number 55 (D4 or Queen 4 in chess notation) has a
;; black knight. If it equals 2, then the piece on square
;; 55 is a white knight.
;;
 ; Initialize the board:
 ;;
(defun initChess ()
  (setq *wking-moved* nil *wrook1-moved* nil *wrook2-moved* nil)
  (setq *bking-moved* nil *brook1-moved* nil *brook2-moved* nil)
  (setq *move-num* 0)
  (dotimes (i 120)
    (setf (aref *board* i)
          (nth i '(
                7   7     7   7   7   7   7   7   7   7   7  ; empty squares around outside of board
                7    7    7   7   7   7   7   7   7   7   7  ; empty squares around outside of board
              4    2    3   5    9   3    2   4   7   7  ; white pieces
              1    1    1   1    1   1    1   1   7   7  ; white pawns
              0    0    0   0    0   0    0   0   7   7  ; 8 blank squares and 2 empty squares 
              0    0    0   0    0   0    0   0   7   7  ; 8 blank squares and 2 empty squares 
              0    0    0   0    0   0    0   0   7   7  ; 8 blank squares and 2 empty squares 
              0    0    0   0    0   0    0   0   7   7  ; 8 blank squares and 2 empty squares 
             -1  -1  -1  -1  -1  -1  -1  -1   7   7     ; black pawns
             -4  -2  -3  -5  -9  -3  -2  -4   7   7     ; black pieces
               7   7   7   7   7   7   7   7   7   7   7   7   7   7  ; empty squares  
               7   7   7   7   7   7   7   7   7   7   7)))))  ; empty squares 


;;
 ;  Print a piece.  If the piece happens to be an empty square
 ; then print the representation for a black or white square on the board:
 ;;

(defun printPiece (piece blackSquare?)
  (if (zerop piece)
    (if blackSquare? (princ " X ") (princ " . "))
    (let ()
      (if (< piece 0)
        (princ " B")
        (princ " W"))
      (princ (nth (- (abs piece) 1)
                  '("P" "N" "B" "R" "Q" "" "" "" "K"))))))

;;
 ;  Print out the entire board:
 ;;

(defun printBoard ()
  (let ((startColumnList '(92 82 72 62 52 42 32 22)))
    (dotimes (i 8)
      (terpri)
      (dotimes (j 8)
        (let* ((boardPos (+ (nth i startColumnList) j))
               (blackSquare?
                (member boardPos
                        '(22 24 26 28 33 35 37 39 42 44 46 48
                          53 55 57 59 62 64 66 68 73 75 77 79
                          82 84 86 88 93 95 97 99))))
          (printPiece (aref *board* boardPos) blackSquare?))))
    (terpri)))

;;
 ; Calculate all possible moves from all squares:
 ;;

(defun posib (&aux returnedMoveList)
  (dotimes (ii 78)
    (let* ((i (+ ii 22))
           (boardVal (aref *board* i)))
      (if (< boardVal 0) ;; valid piece to move?
        ;; collect all squares to which piece on
        ;; square i can move to:
        (dolist (aMove (goto i t))
          (if (and
                ;; check for either an empty space or opponent piece:
               (>= (aref *board* (cadr aMove)) 0)
               (not (equal 7 (aref *board* (cadr aMove)))))
            (setq returnedMoveList
                  (cons aMove returnedMoveList)))))))
  (if (and
       (not *bking-moved*)
       (not *brook2-moved*)
       (equal (aref *board* 97) 0)
       (equal (aref *board* 98) 0)
       (< (aref *human-square-control* 96) 1)
       (< (aref *human-square-control* 97) 1)
       (< (aref *human-square-control* 98) 1))
    (setq returnedMoveList (cons 'oo returnedMoveList)))
  (if (and
       (not *bking-moved*)
       (not *brook1-moved*)
       (equal (aref *board* 95) 0)
       (equal (aref *board* 94) 0)
       (equal (aref *board* 93) 0)
       (< (aref *human-square-control* 96) 1)
       (< (aref *human-square-control* 95) 1)
       (< (aref *human-square-control* 94) 1))
    (setq returnedMoveList (cons 'ooo returnedMoveList)))
  returnedMoveList)

;;
 ; For a given square, return a list of all moves the
 ; piece on that square can move to:
 ;;

(defun goto (squareNum captureFlag
                  &aux piece retList ival pieceType
                  pieceIndex pieceMovementIndex)
  (setq piece (aref *board* squareNum)
        ival '(8 0 3)
        pieceType (abs piece)
        pieceIndex (nth pieceType *index*)
        pieceMovementIndex (nth pieceIndex *piece*))
  (if (not (equal piece 0)) ; make sure that there is a piece on squrae
    (case pieceType
      ((1)  ; PAWN
       (let ((sideIndex (if (< piece 0) -1 +1)))
         (dolist (captureDelta '(11 9)) ;; check for diagonal captures
           (let* ((movementOffsetInBoard (+ squareNum (* sideIndex captureDelta)))
                  (targetPiece (aref *board* movementOffsetInBoard)))
             (if (or
                  (and
                   (<= targetPiece -1) ; enemy piece --> legal capture
                   (not (equal targetPiece 7)) ; not off of board
                   (> piece 0))        ; computer piece moving
                  (and
                   (>= targetPiece 1)  ; computer piece
                   (not (equal targetPiece 7)) ; not off of board
                   (< piece 0)))       ; player piece moving
               (setq retList (cons
                                     (list 
                                       squareNum
                                       (+ squareNum (* sideIndex captureDelta)))
                                    retList)))))
         ;; Check for initial pawn move of two squares forward:
         (let* ((movementOffsetInBoard (+ squareNum (* sideIndex 20))))
           (if (and
                captureFlag
                (equal (aref *board* movementOffsetInBoard) 0) ; move-to sq empty?
                (equal (truncate (/ squareNum 10)) (if (< piece 0) 8 3))
                (if (< piece 0)
                  (equal (aref *board* (- squareNum 10)) 0)
                  (equal (aref *board* (+ squareNum 10)) 0)))
             (setq retList (cons (list squareNum
                                       (+ squareNum (* sideIndex 20)))
                                 retList))))
         (let* ((movementOffsetInBoard (+ squareNum (* sideIndex 10))))
           (if (and
                captureFlag
                (equal (aref *board* movementOffsetInBoard) 0)) ; move-to sq empty?
             (setq retList (cons (list squareNum
                                       (+ squareNum (* sideIndex 10)))
                                 retList))))))
      ((2 3 4 5 9)  ;;  KNIGHT BISHOP ROOK QUEEN KING
       (let* ((pieceType (abs piece))
              (movementTableIndex (nth pieceType *index*))
              (nextToSquare (+ squareNum (nth movementTableIndex *piece*))))
         (loop ;; over movement indices
           (loop
             (if (or
                  (> nextToSquare 99)
                  (< nextToSquare 22)
                  (equal (aref *board* nextToSquare) 7))
               (return)) ;; break out of loop if OFF OF BOARD
             (setq retList (cons (list squareNum nextToSquare ) retList))
             (if (not (equal (aref *board* nextToSquare) 0)) ; last move was a capture,
               (return)) ;; so break out of the loop.
             (if (and
                  (equal pieceType 1)
                  (equal (truncate (/ squareNum 10)) 3))
               (return))
             (if (or
                  (equal pieceType KNIGHT)
                  (equal pieceType KING))
               (return))
             (setq nextToSquare
                   (+ nextToSquare
                      (nth movementTableIndex *piece*))))
           (setq movementTableIndex (+ movementTableIndex 1))
           ;; Lack of further  move segments is indicated
           ;; by a zero in the next element of the move
           ;; index table:
           (if (equal (nth movementTableIndex *piece*) 0)
             (return)) ;; no more move segments
           (setq nextToSquare
                 (+ squareNum
                    (nth movementTableIndex *piece*))))))))
  retList)

;;
 ; Return the static evaluation value for a given board position:
 ;;

(defun value (toSq &aux retVal)
  (setq retVal 0)
  (dotimes (i 100)
    (setf (aref *computer-square-control* i) 0)
    (setf (aref *human-square-control* i) 0))
  ;; Calculate the number of times the computer's
  ;; pieces control each board square:
  (dotimes (ii 78)
    (let ((i (+ ii 22)))
      (if (< (aref *board* i) 0) ;; computer piece
        (let ((moveList (goto i nil))  pawnFudge)
          (dolist (move moveList)
            (if (equal (abs (aref *board* (car move))) 1)
              (setq pawnFudge 1.15)
              (setq pawnFudge 1))
            (setf (aref *computer-square-control* (cadr move))
                  (+ (aref *computer-square-control* (cadr move)) pawnFudge)))))))
  ;; Calculate the number of times the player's
  ;; pieces control each square:
  (dotimes (ii 78)
    (let ((i (+ ii 22)))
      (if (> (aref *board* i) 0) ;; computer piece
        (let ((moveList (goto i nil)) ;; generate moves from square # i
              pawnFudge)
          (dolist (move moveList)
            (if (equal (abs (aref *board* (car move))) 1)
              (setq pawnFudge 1.25)
              (setq pawnFudge 1))
            (setf (aref *human-square-control* (cadr move))
                  (+ (aref *human-square-control* (cadr move)) pawnFudge)))))))
  ;; Subtract 1 from the control array
  ;; for the square being moved to:
  (setf (aref *computer-square-control* toSq)
        (max 0 (- (aref *computer-square-control* toSq) 1)))
  ;; Set initial value based on board control:
  (dotimes (ii 78)
    (let ((i (+ ii 22)))
      (setq retVal
            (+ retVal (* 0.1
                         (- (aref *computer-square-control* i)
                            (aref *human-square-control* i)))))))
  ;; Modify the value based on material advantage,
  ;; square control, and center control:
  (dotimes (ii 78)
    (let ((i (+ ii 22)))
      (if (and
           (not (equal 7 (aref *board* i))) ;; not off of the board
           (not (equal 0 (aref *board* i)))) ;; not a blank square
        (let ((control (- (aref *computer-square-control* i)
                          (aref *human-square-control* i))))
          ;; Piece value:
          (if (< (aref *board* i) 0)
            (setq retVal  ;; computer piece
                  (+ retVal
                     (* (nth (abs (aref *board* i)) *value*)
                        8)))
            (setq retVal  ;; human piece
                  (- retVal
                     (* (nth (abs (aref *board* i)) *value*)
                        8))))
          ;; Check for black piece on white-controlled square:
          (if (and (< control 0) (< (aref *board* i) 0))
            (setq retVal 
                  (- retVal
                     (* 14
                        (nth (abs (aref *board* i)) *value*)))))
          ;; Check for white piece on black-controlled square:
          (if (and (> control 0) (> (aref *board* i) 0))
            (setq retVal
                  (+ retVal
                     (* 2
                        (min ; limit value of attacked piece to 3
                         3   ; points since this side to move next
                         (nth (abs (aref *board* i)) *value*))))))
          ;; King attacked: 
          (if (and (> (aref *human-square-control* i) 0) (equal (aref *board* i) -9))
            (setq retVal (- retVal 5000)))
          ;; Queen attacked:
          (if (and (> (aref *human-square-control* i) 0) (equal (aref *board* i) -5))
            (setq retVal (- retVal 50)))))))
  ;; Pawn placement heuristics:
  (dolist (sq '(55 56 65 66)) ; loop over central four squares
    (if (equal (aref *board* sq) -1)
      (setq retVal (+ retVal 2))) ; black pawn
    (if (equal (aref *board* sq)  1)
      (setq retVal (- retVal 2)))) ; white pawn
  ; Loop over central 16 squares:
  (dolist (sq 
           '(44 45 46 47 54 55 56 57 64 65 66 67 74 75 76 77))
    (if (equal (aref *board* sq) -1) (setq retVal (+ retVal 1))) ; black pawn
    (if (equal (aref *board* sq)  1) (setq retVal (- retVal 1)))) ; white pawn
  ;; Decrease value of moving queen in the first five moves:
  (if (and (< *move-num* 5) (equal (aref *board* toSq) -5))
    (setq retVal (- retVal 10)))
  ;; Decrease value of moving king in the first 15 moves:
  (if (and (< *move-num* 15) (equal (aref *board* toSq) -9))
    (setq retVal (- retVal 20)))
  (+ retVal (random 2)))

;;
 ; Convert internal square number to algrabraic notation:
 ;;

(defun board-pr (sq)
  (let* ((rank (truncate (/ sq 10)))
         (file (- sq (* rank 10))))
    (setq file (- file 2))
    (setq rank (- rank 1))
    (princ (nth file '("A" "B" "C" "D" "E" "F" "G" "H")))
    (princ rank)))

;;
 ; Return a list containing the algrabraic notation for a square.
 ;  For example, square number 22 would be converted to (A 1).
 ;;

(defun board-sq (sq)
  (let* ((rank (truncate (/ sq 10)))
         (file (- sq (* rank 10))))
    (setq file (- file 2))
    (setq rank (- rank 1))
    (list (nth file '("A" "B" "C" "D" "E" "F" "G" "H"))
          rank)))

;;
 ; Find the "best" move:
 ;;

(defun Mover ()
  (setq *move-num* (+ *move-num* 1))
  (let ((possibleMoves (posib))
        (bestMove nil)
        (bestValue -100000)
        (to nil)
        moveValues ;; for debug output only
        tosave fromsave newVal)
    (dolist (pm possibleMoves)
      (setq tosave 0)
      (if (equal pm 'oo)
        (let ()
          (setf (aref *board* 96) 0)
          (setf (aref *board* 97) -4)
          (setf (aref *board* 98) -9)
          (setf (aref *board* 99) 0)
          (setq to 10)) ; off of board
        (if (equal pm 'ooo)
          (let ()
            (setf (aref *board* 96) 0)
            (setf (aref *board* 95) -4)
            (setf (aref *board* 94) -9)
            (setq to 10) ; off of board
            (setf (aref *board* 92) 0))
          (let ()
            (setq fromsave (aref *board* (car pm)))
            (setq tosave (aref *board* (cadr pm)))
            (setf (aref *board* (cadr pm)) (aref *board* (car pm)))
            (setf (aref  *board*(car pm)) 0)
            (setq to (cadr pm)))))
      ;; Call value to calculate a numeric score for
      ;; how "good" this board position is for the computer:
      (setq newVal (value to))
      ;; increase the score slightly for captures:
      (if (> tosave 0)
        (setq newVal
              (+ newVal 12 (* 11 (nth tosave *value*)))))
      (if (member pm '(oo ooo)) (setq newVal (+ newVal 10)))
      (if *debug*
        (if (member pm '(oo ooo))
          (setq moveValues
                (cons
                 (list (list pm) newVal)
                 moveValues))
          (setq moveValues
                (cons
                 (list 
                  (append 
                   (board-sq (car pm)) '(" to ")
                   (board-sq (cadr pm)))
                  newVal)
                 moveValues))))
      (if (> newVal bestValue)
        (let ()
          (setq bestValue newVal)
          (setq bestMove pm)))
      (if (equal pm 'oo)
        (let ()
          (setf (aref *board* 96) -9)
          (setf (aref *board* 97) 0)
          (setf (aref *board* 98) 0)
          (setf (aref *board* 99) -4))
        (if (equal pm 'ooo)
          (let ()
            (setf (aref *board* 96) -9)
            (setf (aref *board* 95) 0)
            (setf (aref *board* 94) 0)
            (setf (aref *board* 92) -4))
          (let ()
            (setf (aref *board* (car pm)) fromsave)
            (setf (aref *board* (cadr pm)) tosave)))))
    (if *debug*
      (let ()
        (dolist (x
                 (sort moveValues
                       #'(lambda (x y) (> (cadr x) (cadr y)))))
          (terpri)
          (dolist (y (car x))
            (princ y))
          (princ " : ")
          (princ (truncate (cadr x))))
        (terpri)))
    (if (< bestValue -1000)
      'checkmate
      bestMove)))

;;
 ; Main driver program:
 ;;
(defun chess (&optional restart)
  (if (not restart) (initChess))
  (if (equal restart 'backup)  ;; debug option to back up one move on restart
    (let ()
      (princ "Backing up the game by one move")
      (terpri)
      (dotimes (i 120)
        (setf (aref *board* i) (aref *old-board* i)))
      (setq *moves-for-current-piece* *old-moves*)))
  (printBoard)
  (loop
    (princ "Enter your move (e.g., d2-d4) : ")
    (let* ((response (string-upcase (princ-to-string (read)))) ;; added string-upcase (differs from book)
           (moved? nil))
      (if (equal response "OO") ;; Common LISP converts reads to uppercase
        (if
          (and  ;; let us be sure that casting king side is legal
           (not *wking-moved*)
           (not *wrook2-moved*)
           (equal (aref *board* 27) 0)
           (equal (aref *board* 28) 0)
           (< (aref *computer-square-control* 26) 1)
           (< (aref *computer-square-control* 27) 1)
           (< (aref *computer-square-control* 27) 1))
          (let ()
            (setq moved? t)
            (princ "Castle King side") (terpri)
            (setf (aref *board* 26) 0) ; blank old king square
            (setf (aref *board* 29) 0) ; blank old rook square
            (setf (aref *board* 27) 4) ; rook
            (setf (aref *board* 28) 9))) ; king
        (if (equal response "OOO") ;; Common LISP converts reads to uppercase
          (if
            (and ;; be sure that a queen side castle is legal
             (not *wking-moved*)
             (not *wrook1-moved*)
             (equal (aref *board* 23) 0)
             (equal (aref *board* 24) 0)
             (equal (aref *board* 25) 0)
             (< (aref *computer-square-control* 26) 1)
             (< (aref *computer-square-control* 25) 1)
             (< (aref *computer-square-control* 24) 1))
            (let ()
              (setq moved? t)
              (princ "Castle Queen side") (terpri)
              (setf (aref *board* 26) 0) ; blank old king square
              (setf (aref *board* 22) 0) ; blank old rook square
              (setf (aref *board* 25) 4) ; rook
              (setf (aref *board* 24) 9))))) ; king
      (if (equal (length response) 5)
        (let* 
           ((fromCol (- (char-code (char response 0)) 63))
              (fromRow (- (char-code (char response 1)) 47))
              (toCol (- (char-code (char response 3)) 63))
              (toRow (- (char-code (char response 4)) 47))
              (from (+ (* fromRow 10) fromCol))
              (to (+ (* toRow 10) toCol)))
          (setq moved? t)
          (setf (aref *board* to) (aref *board* from))
          (setf (aref *board* from) 0)))
      (if (not moved?)
        (print "What???")
        (let ()
          (printBoard)
          ;; Remember last state of the board to allow backing up
          ;; one move for debug by running: (chess 'backup):
          (dotimes (i 120)
            (setf (aref *old-board* i) (aref *board* i)))
          (setq *old-moves* *moves-for-current-piece*)
          ;; Calculate the "best" computer move:
          (let ((bestMove (Mover)))
            (if (equal bestMove 'checkmate)
              (let ()
                (princ "Checkmate!!")
                (terpri)
                (return)))
            (if (equal bestMove 'oo)
              (let ()
                (setf (aref *board* 96) 0)
                (setf (aref *board* 97) -4)
                (setf (aref *board* 98) -9)
                (setf (aref *board* 99) 0)
                (setf *bking-moved* t)
                (setf *brook2-moved* t)
                (terpri) (princ "OO") (terpri))
              (if (equal bestMove 'ooo)
                (let ()
                  (setf (aref *board* 96) 0)
                  (setf (aref *board* 95) -4)
                  (setf (aref *board* 94) -9)
                  (setf (aref *board* 92) 0)
                  (setf *bking-moved* t)
                  (setf *brook1-moved* t)
                  (terpri) (princ "OOO") (terpri))
                (let ()
                  (setf (aref *board* (cadr bestMove)) (aref *board* (car bestMove)))
                  (setf (aref *board* (car bestMove)) 0)
                  (terpri)  (princ "Computer move : ")
                  (board-pr (car bestMove))
                  (princ "-")
                  (board-pr (cadr bestMove))
                  (terpri)))))
          (printBoard))))))
