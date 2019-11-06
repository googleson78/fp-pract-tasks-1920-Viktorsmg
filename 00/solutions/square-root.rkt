#lang racket

(provide my-sqrt)

;Започваме с резултат от 0
;Дигаме го с +1 докато не надминем търсеният квадрат
;Намерили сме прецизност до тази цифра, започваме да дигаме с +0.1 докато не го надминем пак
;... +0.01... +0.001...
(define (my-sqrt x precision)
    (define (approx curr digiti depth)
        (if ( or (> depth precision) (equal? (sq curr) x) )
            curr
            (if (> (sq (add1_dig curr digiti)) x)
                (approx curr (+ digiti 1) (+ depth 1))
                (approx (add1_dig curr digiti) digiti depth)
            )
        )
    )
    (+ (approx 0 0 0) 0.0)
)

(define (my-sqrt x) (my-sqrt x 10))
