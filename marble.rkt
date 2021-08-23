#lang racket

;; Implements a marble maze game
;;
;; To add new elements to the game you need to add a new value 
;; for elements of the matrix to either CURR-RANGE-VALS or NEX-
;;RANGE-VALS (depending on whether they affect the marble when it
;; moves over the position or runs into the position. Then the 
;; new value must have an image associated with it, and the 
;; assoication added to DRAW-BOARD-SQUARE. And then the action
;; to be taken needs to be added to HANDLE-RANGE-VAL

(require 2htdp/universe
         2htdp/image
         (only-in utils/2htdp/image
                  image-center-x
                  image-center-y
                  board-pos->image-pos
                  image-pos->board-pos
                  xy->center-xy)
         (only-in utils/matrix
                  matrix
                  matrix-cols
                  matrix-rows
                  matrix-ref
                  matrix-pos->list-pos)
         (only-in utils/list
                  list->values
                  list-set*))


;;---------------------------------------------------------------
;; Global variables. 
;; These values are concerned with the game matrix and the motion
;; of the marble across the game world.
;;---------------------------------------------------------------


;; current range values are those that have to be landed on to activate
(define CURR-RANGE-VALS '(* h))

;; next range values are those that have to be coming up to activate
(define NEXT-RANGE-VALS '(b))

;; make-board-list: (rows cols dft-val) (row col val) ...
;; A helper macro for buiding a board list. The first triple indicates the number
;; of rows and columns in the board and the default board values. This is followed
;; by 1 or more triples indicating individual board elements. 
(define-syntax-rule (make-board-list (rows cols dft-val) (row col val) ...)
  (list-set* (make-list (* rows cols) dft-val)
             ((matrix-pos->list-pos row 
                                    col 
                                    #:cols-per-row BOARD-COLS) 
              val) ...))

(define BOARD-ROWS 12)
(define BOARD-COLS 13)

;; The queue of baord lists used to construct successive game boards.
(define BOARD-QUEUE
  (list (make-board-list (BOARD-ROWS BOARD-COLS 's)
                         (03 06 'b)
                         (04 10 'b) 
                         (05 03 '*)
                         (06 05 'b) 
                         (07 02 'b) 
                         (08 09 'b))
        
        (make-board-list (BOARD-ROWS BOARD-COLS 's)
                         (00 08 'b)
                         (00 11 'b)
                         (01 03 'b)
                         (02 01 'b)
                         (02 09 'b)
                         (03 07 '*)
                         (05 04 'b)
                         (06 08 'b)
                         (07 12 'b)
                         (08 05 'b)
                         (09 02 'b)
                         (10 09 'b)
                         (11 03 'b)
                         (11 04 'b))
        
        (make-board-list (BOARD-ROWS BOARD-COLS 's)
                         (00 01 'b)
                         (00 07 'b) 
                         (00 11 'b) 
                         (01 03 'b) 
                         (02 06 '*) 
                         (02 09 'b)
                         (03 12 'b)
                         (04 00 'b)
                         (05 04 'b)
                         (06 06 'b)
                         (06 12 'b)
                         (07 01 'b)
                         (07 08 'b)
                         (08 11 'b)
                         (09 04 'b)
                         (09 07 'b)
                         (10 02 'b)
                         (11 00 'b)
                         (11 09 'b))
        (make-board-list (BOARD-ROWS BOARD-COLS 's)
                         (00 01 'b)
                         (00 07 'b) 
                         (00 11 'b) 
                         (01 03 'b) 
                         (02 09 'b)
                         (03 12 'b)
                         (04 00 'b)
                         (05 04 'b)
                         (06 05 'b)
                         (06 12 'b)
                         (07 01 'b)
                         (07 08 'b)
                         (08 00 'b)
                         (08 06 'h)
                         (08 11 'b)
                         (09 04 'b)
                         (09 07 'b)
                         (09 09 'h)
                         (10 09 'b)
                         (11 01 'b)
                         (11 10 '*))))

;; holds a single board matrix
(define BOARD #f)


;;---------------------------------------------------------------
;; Graphic image elements. 
;; These global values are used to draw the game world and 
;; govern the marble's motion across it.
;;---------------------------------------------------------------


;; all game images scale from the marble size
(define MARBLE-SIZE 20)

;; marble image
(define MARBLE 
  (let ([size MARBLE-SIZE])
    (let* ([bottom-img (circle size 'solid 'blue)]
           [top-img ; recursive circular outlines
            (let loop ([size size] [img (circle size 'outline 'silver)])
              (cond
                [(zero? size) img]
                [else (loop (- size 10) 
                            (overlay (circle size 'outline 'darkgreen)
                                     img))]))])
      (overlay top-img bottom-img))))

;; marble final image (game over)
(define MARBLE-STAR 
  (let ([size MARBLE-SIZE])
    (overlay (radial-star 30 size (* size 4/3) 'outline 'darkred)
             (radial-star 30 (* size 2) (* size 1/2) 'outline 'purple)
             (radial-star 30 (* size 4) size 'outline 'gold)
             MARBLE)))

;; board square images
(define SQUARE (color-frame 'black (square (image-width MARBLE) 'solid 'tan)))
(define BUMPER (color-frame 'black (overlay 
                                    (square 28 'solid 'silver)
                                    (square 36 'solid 'darkgreen)
                                    (square 40 'solid 'brown))))
(define HOLE (color-frame 'black (overlay
                                  (circle MARBLE-SIZE 'outline 'silver)
                                  (circle (- MARBLE-SIZE 3) 'solid 'black)
                                  SQUARE)))
(define STAR (overlay (star (* 1/3 (image-width MARBLE)) 'solid 'gold)
                      (star (* 1/2 (image-width MARBLE)) 'solid 'black)
                      SQUARE))

;; A scene crops images placed within it.
(define WIDTH (* (+ BOARD-COLS 2) (image-width SQUARE)))
(define HEIGHT (* (+ BOARD-ROWS 2) (image-height SQUARE)))
(define MT (empty-scene WIDTH HEIGHT 'black))

;; This value is calculated to be proportional to the square size.
(define SPEED (quotient (* 4 (image-width SQUARE)) 10))


;;---------------------------------------------------------------
;; Helper functions.
;; These functions are concerned with conversions between the game
;; image and the board matrix, and the creation and placement of 
;; the marble and board.
;;---------------------------------------------------------------


;; The world state. 
;;    x - marble's x-position on the board
;;    y - marble's y-position on the board
;;    dir - marble motion direction: "" for no motion.
;;    init-row - marble initial board row
;;    init-col - marble initial board column
;;    transition? - indicates whether a new board request is pending
(struct marble (x y dir init-row init-col transition?) #:mutable #:transparent)

;; cener-marble: marble row column width height -> marble?
;; Returns marble centered on the board's row column position using the
;; image provided to board row/column into board x/y pixel location.
(define (center-marble m r c w h)
  (define-values (X Y) (list->values (board-pos->image-pos r c w h)))
  (set-marble-x! m X)
  (set-marble-y! m Y)
  (set-marble-dir! m "")
  m)

;; new-marble: row col -> marble?
;; Returns a new instance of marble centered at board row/column. 
(define (new-marble row col)
  (center-marble (marble 0 0 "" row col #f) 
                 row 
                 col 
                 (image-width SQUARE)
                 (image-height SQUARE)))

;; center-marble-for-bumper: marble row column -> marble?
;; Returns marble centered on the board's row column position.
(define (center-marble-for-bumper m r c)
  (center-marble m r c (image-width SQUARE) (image-height SQUARE)))

;; center-marble-for-star: marble row column -> marble?
;; Returns marble centered on the board's row column postion, and 
;; requesting a new board.
(define (center-marble-for-star m r c)
  (define M (center-marble m r c (image-width SQUARE) (image-height SQUARE)))
  (set-marble-transition?! M #t)
  M)

;; new-board: [marble] [marble-col] [marble-row] -> marble?
;; Sets up a new board and returns the appropriate marble.
(define (new-board (ws #f)
                   #:marble-row (marble-row #f)
                   #:marble-col (marble-col #f))
  (set! BOARD (matrix (first BOARD-QUEUE)
                      #:rows BOARD-ROWS
                      #:cols BOARD-COLS))
  (set! BOARD-QUEUE (cdr BOARD-QUEUE))
  (cond 
    [(and marble-row marble-col)
     (new-marble marble-row marble-col)]
    [ws 
     (define-values (CURR-ROW CURR-COL) 
       (list->values (image-pos->board-pos (marble-x ws) 
                                           (marble-y ws) 
                                           (image-width SQUARE)
                                           (image-height SQUARE))))
     (sleep 1)
     (new-marble CURR-ROW CURR-COL)]
    [else (new-marble 0 0)]))


;;---------------------------------------------------------------
;; on-tick functions.
;; These functions are involved in generating a new marble from
;; the current one, either through motion or through contact with
;; a special square.
;;---------------------------------------------------------------


;; next-marble: marble [board] [image] -> marble?
;; Returns the next marble for this clock tick. 
(define (next-marble ws 
                     (board BOARD) 
                     (width (image-width SQUARE))
                     (height (image-height SQUARE)))
  (define CURR-X (marble-x ws))
  (define CURR-Y (marble-y ws))
  (define CURR-D (marble-dir ws))
  (define-values (CURR-ROW CURR-COL) 
    (list->values 
     (image-pos->board-pos (marble-x ws) (marble-y ws) width height)))
  (define CENTER-POSN
    (center-marble (new-marble 0 0) CURR-ROW CURR-COL width height))
  (define CENTER-X (cond ; offset center by the speed
                     [(string=? CURR-D "left") (+ (marble-x CENTER-POSN) SPEED)]
                     [(string=? CURR-D "right") (- (marble-x CENTER-POSN) SPEED)]
                     [else (marble-x CENTER-POSN)]))
  (define CENTER-Y (cond ; offset center by the speed
                     [(string=? CURR-D "up") (+ (marble-y CENTER-POSN) SPEED)]
                     [(string=? CURR-D "down") (- (marble-y CENTER-POSN) SPEED)]
                     [else (marble-y CENTER-POSN)]))
  (define NEXT-ROW (let ([val (cond
                                [(string=? CURR-D "up") (sub1 CURR-ROW)]
                                [(string=? CURR-D "down") (add1 CURR-ROW)]
                                [else CURR-ROW])])
                     (cond
                       [(< val 0) CURR-ROW]
                       [(> val (matrix-rows board)) CURR-ROW]
                       [else val])))
  (define NEXT-COL (let ([val (cond
                                [(string=? CURR-D "left") (sub1 CURR-COL)]
                                [(string=? CURR-D "right") (add1 CURR-COL)]
                                [else CURR-COL])])
                     
                     (cond
                       [(< val 0) CURR-COL]
                       [(> val (matrix-cols board) CURR-COL)]
                       [else val])))
  (define CURR-VAL (cond
                     [(< CURR-COL 0) 'h]
                     [(>= CURR-COL (matrix-cols board)) 'h]
                     [(< CURR-ROW 0) 'h]
                     [(>= CURR-ROW (matrix-rows board)) 'h]
                     [else (matrix-ref board CURR-ROW CURR-COL)]))
  (define NEXT-VAL (cond
                     [(< NEXT-COL 0) 'h]
                     [(>= NEXT-COL (matrix-cols board)) 'h]
                     [(< NEXT-ROW 0) 'h]
                     [(>= NEXT-ROW (matrix-rows board)) 'h]
                     [else (matrix-ref board NEXT-ROW NEXT-COL)]))
  (define RANGE-VAL 
    (val-in-range CURR-X CURR-Y CURR-D width height CURR-VAL NEXT-VAL))
  
  (cond
    ; marble needs board transition
    [(marble-transition? ws) (new-board ws)]
    
    ; the marble's not moving, return the marble
    [(string=? CURR-D "") ws]
    
    ; in range of a "special" square. Hangle it.
    [RANGE-VAL (handle-range-val ws CURR-ROW CURR-COL RANGE-VAL)]
    
    ; heading left, change the x-coord by speed
    [(string=? CURR-D "left") (set-marble-x! ws (- CURR-X SPEED)) ws]
    
    ; heading right, change the x-coord by speed. 
    [(string=? CURR-D "right") (set-marble-x! ws (+ CURR-X SPEED)) ws]
    
    ; heading up, change the y-coord by speed
    [(string=? CURR-D "up") (set-marble-y! ws (- CURR-Y SPEED)) ws]
    
    ; heading down, change the y-coord by speed
    [(string=? CURR-D "down") (set-marble-y! ws (+ CURR-Y SPEED)) ws]
    
    ; do nothing, return the marble
    [else ws]))

;; val-in-range: x y dir width height curr next
;; Returns the value from the appropriate current/next range vals list 
;; that is "within range" for handling that square's feature. If the 
;; marble is not in range of a special square then returns #f.
(define (val-in-range x y dir width height curr next)
  (define-values (RAW-CENTER-X RAW-CENTER-Y) 
    (list->values (xy->center-xy x y width height)))
  
  (define ADJ-CENTER-X (cond ; offset center by the speed
                         [(string=? dir "left") (+ RAW-CENTER-X 
                                                   (quotient SPEED 2))]
                         [(string=? dir "right") (- RAW-CENTER-X 
                                                    (quotient SPEED 2))]
                         [else RAW-CENTER-X]))
  
  (define ADJ-CENTER-Y (cond ; offset center by the speed
                         [(string=? dir "up") (+ RAW-CENTER-Y 
                                                 (quotient SPEED 2))]
                         [(string=? dir "down") (- RAW-CENTER-Y 
                                                   (quotient SPEED 2))]
                         [else RAW-CENTER-Y]))
  
  (define IN-RANGE? (cond
                      ; heading left with marble in range of square's center.
                      [(and (string=? dir "left")
                            (<= x ADJ-CENTER-X))
                       #t]
                      
                      ; heading right with marble in range of square's center.
                      [(and (string=? dir "right")
                            (>= x ADJ-CENTER-X))
                       #t]
                      
                      ; heading up with marble in range of square's center.
                      [(and (string=? dir "up")
                            (<= y ADJ-CENTER-Y))
                       #t]
                      
                      ; heading down with marble in range of square's center.
                      [(and (string=? dir "down")
                            (>= y ADJ-CENTER-Y))
                       #t]
                      
                      ; the marble is not moving and in range of square's center.
                      [else #f]))
  (define RANGE-VAL (cond
                      [(member curr CURR-RANGE-VALS) => first]
                      [(member next NEXT-RANGE-VALS) => first]
                      [else #f]))
  
  (if (and IN-RANGE? RANGE-VAL)
      RANGE-VAL
      #f))

;; handle-range-val: s row col range-val
;; handles what should happen when the marble is in range of a special square.
(define (handle-range-val ws row col range-val)
  (match range-val
    ['* (center-marble-for-star ws row col)]
    ['h (new-marble (marble-init-row ws) (marble-init-col ws))]
    ['b (center-marble-for-bumper ws row col)]
    [else 
     (error (format "handle-range-value: range-val ~s not handled." range-val))]))


;;---------------------------------------------------------------
;; on-key functions.
;; These functions are concerned with capturing key-strokes and 
;; setting the world state to the appropriate value.
;;---------------------------------------------------------------


;; direct-marble: marble key -> marble?
;; Set's the marble in motion: "left", "right", "up", "down".
(define (direct-marble ws ke)
  (cond
    [(marble-transition? ws) ws]
    [(not (string=? (marble-dir ws) "")) ws]
    [(or (key=? ke "left")
         (key=? ke "right")
         (key=? ke "up")
         (key=? ke "down")) 
     (set-marble-dir! ws ke) ws]
    [else ws]))

;;---------------------------------------------------------------
;; to-draw functions.
;; These functions are concerned with drawing the game world. We
;; break the drawing process up into world, board, row, and square 
;; prpocesses. 
;;---------------------------------------------------------------

;; draw-marble-world: ws -> image?
;; Draws the board and marble.
(define (draw-marble-world ws)
  (define BOARD-SCENE (place-image MARBLE
                                   (marble-x ws)
                                   (marble-y ws)
                                   (draw-board BOARD)))
  (place-image BOARD-SCENE
               (image-center-x MT)
               (image-center-y MT)
               MT))

;; draw-board: board -> image?
;; Draws the board based on the board matrix.
(define (draw-board board)
  (let loop ([board board] [acc empty])
    (cond
      [(null? board) (apply above (reverse acc))]
      [else (loop (cdr board) (cons (draw-board-row (car board)) acc))])))

;; draw-board-row: row -> image?
;; Draws a row of the board.
(define (draw-board-row row)
  (let loop ([row row] [acc empty])
    (cond
      [(null? row) (apply beside (reverse acc))]
      [else (loop (cdr row) (cons (draw-board-square (car row)) 
                                  acc))])))

;; draw-board-square v -> image?
;; Draws a board square representing the board's column and row element.
(define (draw-board-square v)
  (cond
    [(eq? v '*) STAR]
    [(eq? v 'h) HOLE]
    [(eq? v 'b) BUMPER]
    [else SQUARE]))


;;---------------------------------------------------------------
;; stop-when functions.
;; These functions are concerned with determining when the game
;; is over. 
;;---------------------------------------------------------------


;; no-more-boards? marble -> boolean?
;; Indicates whether the marble is asking for a board transition, but no
;; more are available.
(define (no-more-boards? ws)
  (and (marble-transition? ws)
       (empty? BOARD-QUEUE)))

;; draw-final-world marble -> image?
;; Draws the final board replacing MARBLE with MARBLE-STAR at the marble's
;; final positon. 
(define (draw-final-world ws)
  (define BOARD-SCENE (place-image MARBLE-STAR
                                   (marble-x ws)
                                   (marble-y ws)
                                   (draw-board BOARD)))
  (place-image BOARD-SCENE
               (image-center-x MT)
               (image-center-y MT)
               MT))

;; star-game -> marble
;; Launches Big-Bang.
(define (start-game)
  (big-bang (new-board #:marble-row 6 #:marble-col 8)
            (on-tick next-marble)
            (on-key direct-marble)
            (to-draw draw-marble-world)
            (stop-when no-more-boards? draw-final-world)
            (name "Marble Maze"))
  
  (void))

(start-game)
