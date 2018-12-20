#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (if (= n 1)
      (cons (car (s)) null)
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (lambda ()
    (letrec ([f (lambda (x)
                  (let ([s (lambda () (f (+ x 1)))])
                    (if (= 0 (remainder x 5))
                        (cons (- x) s)
                        [cons x s])))])
      (f 1))))

(define ones (lambda () (cons 1 ones)))

(define dan-then-dog
  (lambda ()
    (letrec ([f (lambda (dan)
                  (cons (if dan "dan.jpg" "dog.jpg")
                        (lambda () (f (not dan)))))])
      (f #t))))

(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s)))
          (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n)
                            (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([iter (lambda (n)
                  (cond [(>= n (vector-length vec)) #f]
                        [(pair? (vector-ref vec n))
                         (if (= v (car (vector-ref vec n)))
                             (vector-ref vec n)
                             (iter (+ n 1)))]
                        [#t (iter (+ n 1))]))])
    (iter 0)))

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [pos 0])
    (lambda (v)
      (let ([val (vector-assoc v cache)])
        (if val
            val
            (let ([val (assoc v xs)])
              (if val
                  (begin (vector-set! cache pos val)
                         (set! pos (remainder (+ pos 1) n))
                         val)
                  #f)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([a e1])
       (letrec ([loop (lambda ()
                        (let ([b e2])
                          (if (< b a)
                              (begin b (loop))
                              #t)))])
         (loop)))]))
