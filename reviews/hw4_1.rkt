
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (let* ([len (length xs)]
         [rem (remainder n len)])
    (cond
      [(< n 0)   (error "list-nth-mod: negative number")]
      [(= rem 0) (car xs)]
      [#t        (car (list-tail xs rem))])))

;; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (lambda ()
                  (if (= (remainder x 5) 0)
                      (cons (- x) (f (+ x 1)))
                      (cons x (f (+ x 1))))))])
    (f 1)))

;; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (lambda ()
                  (cons (if x "dan.jpg" "dog.jpg")
                        (f (not x)))))])
    (f #t)))

;; Problem 7
(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s)))
          (stream-add-zero (cdr (s))))))

;; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (lambda ()
                  (cons (cons (list-nth-mod xs n)
                              (list-nth-mod ys n))
                        (f (+ n 1)))))])
    (f 0)))

;; Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec))
                    #f
                    (let ([elem (vector-ref vec n)])
                      (if (and (pair? elem) (equal? (car elem) v))
                          elem
                          (f (+ n 1))))))])
    (f 0)))

;; Problem 10
(define (cached-assoc xs n)
  (let ([vec  (make-vector n #f)]
        [slot 0])
    (lambda (v)
      (let ([cache_lookup (vector-assoc v vec)])
        (if cache_lookup
            cache_lookup
            (let ([list_lookup (assoc v xs)])
              (if list_lookup
                  (begin
                    (vector-set! vec slot list_lookup)
                    (if (= slot (- (vector-length vec) 1))
                        (set! slot 0)
                        (set! slot (+ slot 1)))
                    list_lookup)
                  #f)))))))
      
;; Problem 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([condition e1]
              [body      (lambda () e2)]
              [loop      (lambda ()
                           (if (< (body) condition)
                               (loop)
                               #t))])
       (loop))]))
