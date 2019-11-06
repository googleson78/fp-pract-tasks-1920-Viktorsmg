#lang racket

(provide my-sqrt)

;Започваме с резултат от 0
;Дигаме го с +1 докато не надминем търсеният квадрат
;Намерили сме прецизност до тази цифра, започваме да дигаме с +0.1 докато не го надминем пак
;... +0.01... +0.001...

(define (sq x) (* x x))

(define (add1_dig n dig)
    (+ n (/ 1 (expt 10 dig)))
)

(define (my-sqrt x . precision)
    (define (approx curr digiti depth limit)
        (if ( or (> depth limit) (equal? (sq curr) x) )
            curr
            (if (> (sq (add1_dig curr digiti)) x)
                (approx curr (+ digiti 1) (+ depth 1) limit)
                (approx (add1_dig curr digiti) digiti depth limit)
            )
        )
    )

    (if (null? precision)
        (+ (approx 0 0 0 5) 0.0)
        (if (number? precision)
            (+ (approx 0 0 0 precision) 0.0)
            (if (number? (car precision))
                (+ (approx 0 0 0 (car precision)) 0.0)
                (+ (approx 0 0 0 5) 0.0)
            )
        )
    )
    
)