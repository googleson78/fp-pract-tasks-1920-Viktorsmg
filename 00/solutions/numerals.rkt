#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))

(define (succ n)
  (lambda (f v)
    (f (n f v))))

(define (1+ n) (+ n 1))

(define (from-numeral n) (n 1+ 0))

(define (to-numeral n) 
    (if (<= n 0)
        (lambda (f v) v)
        (lambda (f v) 
            (f ((to-numeral (- n 1)) f v) )
        )
    )
)

(define (plus a b)
    (lambda (f v)
        (a f (b f v))
    )
)

(define (mult a b)
    (lambda (f v1) 
        (a (lambda (v2) (b f v2)) v1)
    )
)




(define (make-lambda-pair lm1 lm2)
    (lambda (choice f v)
        ((choice lm1 lm2) f v)
    )
)

(define (lambda-pair-car pair) 
    (lambda (f v) 
        (pair (lambda (lm1 lm2) lm1) f v)
    )
)

(define (lambda-pair-cdr pair) 
    (lambda (f v) 
        (pair (lambda (lm1 lm2) lm2) f v)
    )
)

;Special succ that utilizes lambda pairs to have memory
(define (lambda-pair-succ-memory pair)
    (make-lambda-pair (succ (lambda-pair-car pair)) (lambda-pair-car pair))
)

(define (pred a)
    (lambda (f v) ;                                             counter \/   \/ what we use when the numeral is zero   (I wanted to put "useless word salad" here)
        ((lambda-pair-cdr (a lambda-pair-succ-memory (make-lambda-pair zero zero))) f v)
    )
)
