#lang racket
(provide clearFields
         showDialog
         closeDialog
         joinIdAndName
         IdAndName
         remove-nth)

(define IdAndName #f)
(define (joinIdAndName ids teams)
  (set! IdAndName (map (lambda (id team)
                         (string-append team " (ID: " id ")"))
                       ids teams)))

(define (clearFields . l)
  (map (lambda (el)
         (send el set-value ""))
       l))

(define (showDialog x)
  (send x show #t))

(define (closeDialog x)
  (send x show #f))

(define (remove-nth lst n)
  (let loop ([i 0] [lst lst])
    (cond [(= i n) (rest lst)]
          [else (cons (first lst) (loop (add1 i) (rest lst)))])))