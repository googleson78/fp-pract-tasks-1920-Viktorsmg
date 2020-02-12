#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)


; Returns s if s is a winner, #f otherwise
(define (winnerS b s)
    (define (winln ln)
        (all? (lambda (x) (equal? x s)) ln)
    )
    (if 
    (ormap (lambda (ln) (any? winln ln)) (list (rows b) (diags b) (cols b))); Much harder to read than below text *to me*
    ;(or                                          Fun fact: both have almost the exact same character count (71\/ vs 72^), barring new lines and such
    ;   (any? winln (rows b))
    ;   (any? winln (cols b))
    ;   (any? winln (diags b)))
    s #f)
)

(define (anymat? p? xss)
  (any? (lambda (xs) (any? p? xs)) xss)
)

(define (winner b)
  (or 
    (winnerS b "X") 
    (winnerS b "O") 
    (if (anymat? (lambda (x) (equal? x #f)) b) #f "D")
  )
)

(define (winner-val b s)
  (let ((w (winner b)))
    (cond 
      ((equal? w #f) #f)
      ((equal? w "D") 0)
      ((equal? w s) 1)
      (else -1)
    )
  )
)

(define (inc-pair p)
  (if (equal? (cdr p) 2)
    (if (equal? (car p) 2)
      #f
      (cons (+ (car p) 1) 0)
    )
    (cons (car p) (+ (cdr p) 1))
  )
)

(define (next-free b curr)
  (let ((next (inc-pair curr)))
    (if next
      (if (matrix-refp b next)
        (next-free b next)
        next
      )
      #f
    )
  )
)

(define (first-free b)
  (if (matrix-refp b (cons 0 0))
    (next-free b (cons 0 0))
    (cons 0 0)
  )
)

(define (placep xss p x)
  (place xss (car p) (cdr p) x)
)

(define (matrix-refp xss p)
  (matrix-ref xss (car p) (cdr p))
)

(define (flip x)
  (if (equal? x "X") "O" "X")
)

; Returns 1, 0, -1 for the board, currpos and has0 are internal "variables" (use first-free and -1)
(define (play-val b s currpos has0)
  (let ((w (winner-val b s)))
    (or w
      (if currpos
        (let ((p (play-val (placep b currpos s) (flip s) (first-free (placep b currpos s)) -1)))
          (cond
            ((equal? p -1) 1)
            ((equal? p 0) (play-val b s (next-free b currpos) 0))
            (else (play-val b s (next-free b currpos) has0))
          )
        )
        has0
      )
    )
  )
)

;Returns a position in which to play, and its score, currpos and best are internal "variables" (use first-free and (cons -2 "blablo"))
(define (play-find b s currpos best)
  (if currpos
    (let ((p (play-val (placep b currpos s) (flip s) (first-free (placep b currpos s)) -1)))
      (cond 
        ((equal? p -1) (cons 1 currpos))
        ((equal? p 0) (play-find b s (next-free b currpos) (cons 0 currpos)))
        (else (if (equal? (car best) 0) 
          (play-find b s (next-free b currpos) best)
          (play-find b s (next-free b currpos) (cons -1 currpos))
        ))
      )
    )
    best
  )
)

(define (play b s_bool)
  (let ((s (if s_bool "X" "O")))
    (if (winner b)
      (cons -1 -1)
      (cdr (play-find b s (first-free b) (cons -2 "GlobGloGabGalab")))
    )
  )
)
