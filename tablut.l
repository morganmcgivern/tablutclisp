;morgan mcgivern 2020
(defun tablut ()
  (format t "loading...")
  (setf *columns* '(a b c d e f g h i))
  (setf *rows* (iota 9))
  (wrought-tablut-board)
  (standard-tablut-setup)
  (format t "loaded ~%")
  (display-board)
  nil
  )

(defun standard-tablut-setup ()
  (mapcar #'(lambda (x) (place-piece (first x) (second x) (third x)))
          '(
            (d 1 r) (e 1 r) (f 1 r) (e 2 r) (e 3 w) (e 4 w)
            (a 4 r) (a 5 r) (a 6 r) (b 5 r) (c 5 w) (d 5 w)
            (d 9 r) (e 9 r) (f 9 r) (e 8 r) (e 7 w) (e 6 w)
            (i 4 r) (i 5 r) (i 6 r) (h 5 r) (g 5 w) (f 5 w)
            (e 5 k)
           )
          )
  )

(defun wrought-tablut-board ()
  (setf board (mapcan #'create-board *rows*))
  )

(defun create-board (row &aux rows)
  (setf rows *columns*)
  (setf row-duplicates (duplicate (length rows) row))
  ;(mapcar #'(lambda (a b) (cons a (cons b '()))) suits suit-duplicates)
  ;(mapcar #'cons suits suit-duplicates)
  (mapcar #'(lambda (l1 l2) (cons (list l1 l2) 'u)) rows row-duplicates)
  )

(defun alphab-num (x)
  ; c -> 3
  (+ (position x '(a b c d e f g h i j k l m n o p q r s t u v w x y z) :test #'eq) 1)
  )

(defun alphab-char (num)
  ; 3 -> c
  (nth (- num 1) '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  )

(defun place-piece (x y p)
  ; places p piece at letter x(a character), row y
  (setf (cdr (car (member (cons (list x y) 'u) board :test #'equal))) p)
  )

(defun applicable-geom-move (x y xdest ydest)
  (cond
    ((= (- xdest x) 0) t)
    ((= (- ydest y) 0) t)
    )
  )

(defun piece-exists-atp (x y)
  (if (eq (what-piece-is-at x y) 'u)
    nil
    t)
  )
(defun piece-exists-atlp (l)
  (if (eq (what-piece-is-atl l) 'u)
    nil
    t)
  )

(defun what-piece-is-at (x y)
  (cdr (nth (position (list x y) (mapcar #'car board) :test #'equal) board))
  )

(defun what-piece-is-atl (l)
  (cdr (nth (position l (mapcar #'car board) :test #'equal) board))
  )

(defun king-center-rulep (xdest ydest p)
    (if (and (eq xdest 'h) (= ydest 5) (eq p 'k))
      nil
      t)
  )

(defun capture-trigger (x y p)
  ;x y being the piece initiating a capture's location, p is it's type
  ;could also simply look at entire board state for capture states
  ;wall captures etc
  ;check for a surround capture as well
  )

(defun capture-piece (x y)
  )

(defun surround-capture (pows)
  )

(defun list-line-of-squares (x y x2 y2) ;n n n n
  (snoc
   (nth (position (list (alphab-char x) y) (mapcar #'car board) :test #'equal) board)
   (cond
     ((> y2 y) (line-of-squares x (+ y 1) x2 y2)) ;south
     ((> x2 x) (line-of-squares (+ x 1) y x2 y2)) ;east
     ((< x2 x) (line-of-squares (- x 1) y x2 y2)) ;west
     ((< y2 y) (line-of-squares x (- y 1) x2 y2))) ;north
   )
  )

(defun applicable-move (x y xdest ydest)
  (and
   (not (and (= (alphab-num x) (alphab-num xdest)) (= y ydest)))
   (piece-exists-atp x y)
   (applicable-geom-move (alphab-num x) y (alphab-num xdest) ydest)
   (not (car (member t (mapcar #'piece-exists-atlp (mapcar #'car (rdc (line-of-squares (alphab-num x) y (alphab-num xdest) ydest)))))))
   (king-center-rulep xdest ydest (what-piece-is-at x y))
   )
  )

(defun move-piece (x y xdest ydest)
  (if (applicable-move x y xdest ydest)
    (let*
      (
       (old (position (list x y) (mapcar #'car board) :test #'equal))
       (new (position (list xdest ydest) (mapcar #'car board) :test #'equal))
       (p (cdr (nth old board)))
       )
      (setf (cdr (nth old board)) 'u)
      (setf (cdr (nth new board)) p)
      ;TODO capture trigger check here
      (display-board)
      )
    )
  )

; (defun place-line-piece (x y x2 y2 p)
;   ;(mapcar #'(lambda (x) (place-piece (first x) (second x) (third x))) '((a 2 w) (a 3 w) (a 4 w)))
;   )

;x's for decoration, the king just has to touch a board edge
;they become forts in hnefatafl

;  a b c d e f g h i
; 1 x     r r r     x
; 2         r
; 3         w
; 4 r       w       r
; 5 r r w w K w w r r
; 6 r       w       r
; 7         w
; 8         r
; 9 x     r r r     x

(defun display-board ()
  (format nil " ~%

y x >
v
  a b c d e f g h i
1~{ ~S~} 1
2~{ ~S~} 2
3~{ ~S~} 3
4~{ ~S~} 4
5~{ ~S~} 5
6~{ ~S~} 6
7~{ ~S~} 7
8~{ ~S~} 8
9~{ ~S~} 9
  a b c d e f g h i
~%
" (mapcar #'cdr (subseq board 0 9))
     (mapcar #'cdr (subseq board 9 18))
     (mapcar #'cdr (subseq board 18 27))
     (mapcar #'cdr (subseq board 27 36))
     (mapcar #'cdr (subseq board 36 45))
     (mapcar #'cdr (subseq board 45 54))
     (mapcar #'cdr (subseq board 54 63))
     (mapcar #'cdr (subseq board 63 72))
     (mapcar #'cdr (subseq board 72 81))
          )
  )



;Helper Functions
(defun snoc (a b)
  (if (null b) (list a)
    (cons (car b) (snoc a (cdr b)))
    )
  )
(defun duplicate (n l)
  (if (= n 0) ()
    (snoc l (duplicate (- n 1) l)))
  )
(defun iota (l)
  (if (= l 0) '()
    (snoc l (iota (- l 1)))
    )
  )
  (defun singleton-p (l)
    (= (length l) 1)
  )
  (defun rac (l)
    (cond
      ((null l) (cdr l))
      ((singleton-p l) (car l))
      (T (rac (cdr l)))
    )
  )

(defun rdc (l)
  (cond
    ((null l) '())
    ((singleton-p l) '())
    (T (cons (car l) (rdc (cdr l))))
    )
  )
