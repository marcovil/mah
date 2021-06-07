#lang racket

(define (numsFrom n) (lambda () (cons n (numsFrom (+ n 1)))))

(define nums (numsFrom 0))

;; nums ;; torna una procedura
;; (nums) ;; la valuta
;; (cdr (nums)) ;; valuta la procedura, prende la procedura successiva
;; ecc.

(define (take n xs)
  (cond
    ((equal? n 0) null)
    ((equal? (xs) null) null)
    (true (cons (car (xs)) (take (- n 1) (cdr (xs)))))))

(take 20 nums)

(define (lazymap f xs)
  (if (equal? (xs) null)
      xs
      (lambda () (cons (f (car (xs)))
                       (lazymap f (cdr (xs)))))))

(take 20 (lazymap (lambda (x) (* x x)) nums))