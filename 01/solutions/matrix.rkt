#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

; 00.
; Faster but longer solution
(define (all? p? xs)
    (if (null? xs)
        #t
        (and (p? (car xs)) (all? p? (cdr xs)))
    )
)

; 01.
; Shorter but slower
(define (any? p? xs)
    (foldl (lambda (acc new) (or new (p? acc))) #f xs)
)

; 02.
(define (concat xss) (foldr append '() xss))

; 03.
(define (rows xss) xss)


; 04.

(define (atob a b)
    (if (> a b)
        '()
        (cons a (atob (+ a 1) b))
    )
)

;(define (cols xss) (apply map list xss))
(define (cols xss) (foldr (lambda (idx res) (cons (foldr (lambda (new acc) (cons (list-ref new idx) acc)) '() xss) res)) '() (atob 0 ( - (length (car xss)) 1))))

; 05.
(define (matrix-ref xss i j)
    (list-ref (list-ref xss i) j)
)

; 06.
(define (set xs i x)
    (append (take xs i) (cons x (drop xs (+ i 1))))
)

; 07.
(define (place xss i j x)
    (set xss i (set (list-ref xss i) j x))
)

; 08.
(define (diag xss)
    (cdr (foldr
        (lambda (new acc) (cons (- (car acc) 1) (cons (list-ref new (car acc)) (cdr acc))))
        (cons (- (length (car xss)) 1) '())
        xss))
)

; 09.
(define (diagSec xss)
    (cdr (foldr
        (lambda (new acc) (cons (+ (car acc) 1) (cons (list-ref new (car acc)) (cdr acc))))
        (cons 0 '())
        xss))
)


(define (diags xss) (list (diag xss) (diagSec xss)))

; 10.
(define (map-matrix f xss)
    (map (lambda (xs) (map f xs)) xss)
)

; 11.
(define (filter-matrix p xss)
    (map (lambda (xs) (filter p xs)) xss)
)

; 12.
(define (zip-with f xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))
    )
)

; 13.
(define (zip-matrix xss yss)
    (zip-with (lambda (xs ys) (zip-with cons xs ys)) xss yss)
)
