
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(require rackunit)
;; put your code below

;; - 1 -
; SIGNATURE: number number number -> number list
; REQUIRE:   stride > 0
; EFFECTS:   return list of numbers in the range [low,high] separated by
;            stride and in sorted order from low to high          
(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

;; - 2 -
; SIGNATURE: (string list) string -> string list
; EFFECTS:  return xs with suffix added to every string
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; - 3 -
; SIGNATURE: (a' list) number -> a'
; EFFECTS: return i-th element of xs where i is reminder 
;          of dividing n by xs length
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; - 4 -
; SIGNATURE: (a' stream) number -> (a' list)
; REQUIRE: n > 0
; EFFECTS: return list of first n values produced by s stream
(define (stream-for-n-steps s n)
  (if (zero? n)
      null
      (let ([next_s (s)])
        (cons (car next_s) (stream-for-n-steps (cdr next_s) (- n 1))))))

;; - 5 -
; SIGNATURE: number stream
; EFFECTS: stream of natural numbers except numbers divisible by 5 are negated
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0)
                          (- 0 x)
                          x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; - 6 -
; SIGNATURE: string stream
; EFFECTS: stream that alternate "dan.jpg" and "dog.jpg" strings
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if x
                    (cons "dan.jpg" (lambda () (f #f)))
                    (cons "dog.jpg" (lambda () (f #t)))))])
    (lambda () (f #t))))

;; - 7 -
; SIGNATURE: 'a stream -> (number * 'a) stream
; EFFECTS: takes a stream and produce another stream that produce
;          same values as stream given paired with 0
(define (stream-add-zero s)
  (let ([p (s)])
  (lambda () (cons (cons 0 (car p)) (stream-add-zero (cdr p))))))

;; - 8 -
; SIGNATURE: 'a list 'b list -> ('a * 'b) stream
; REQUIRE: xs and ys are nonempty
; EFFECTS: produce stream with pairs of values of xs and ys in order,
;          that cycles forever through the lists.
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
             (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;; - 9 -
; SIGNATURE: 'a vector -> (pair or boolean)
; EFFECTS: return the first element of vec whose car is equal to v.
;          If such an element doesn't exist return #f
(define (vector-assoc v vec)
  (letrec ([vec-len (vector-length vec)]
           [f (lambda (x)
                (cond [(= vec-len x) #f]
                      [(and (pair? (vector-ref vec x))
                            (equal? (car (vector-ref vec x)) v))
                       (vector-ref vec x)]
                      [#t (f (+ x 1))]))])
    (f 0)))

;; - 10 -
; SIGNATURE: list number -> 'a -> (pair or boolean)
; EFFECTS: takes list and size of cache and return function
;          that takes one argument v and return the first element of xs
;          whose car is equal to v or false if such an element doesn't exist
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [max-pos (- n 1)]
           [cur-pos 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (begin
                          (vector-set! cache cur-pos new-ans)
                          (if (= cur-pos max-pos)
                              (set! cur-pos 0)
                              (set! cur-pos (+ cur-pos 1)))
                          new-ans)))))])
    f))

;; - 11 -
;; EFFECTS: macro that is used like (while-less e1 do e2)
;          where e1 and e2 are expressions.
;          It keeps evaluating e2 until it's less than e1.
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([e e1]
              [loop (lambda (it)
                      (if (< it e)
                          (loop e2)
                          #t))])
       (loop e2))]))