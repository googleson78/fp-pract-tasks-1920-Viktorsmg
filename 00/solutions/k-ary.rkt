#lang racket

(provide from-k-ary
         to-k-ary)

(define (from-k-ary n k)
    (if (< n k)
        n
        (+ (* (from-k-ary (quotient n 10) k) k) (remainder n 10))
    )
)

(define (to-k-ary n k) 
    (if (< n k)
        n
        (+ (* (to-k-ary (quotient n k) k) 10) (remainder n k))
    )
)
