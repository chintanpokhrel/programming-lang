
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define sequence
  (lambda (low high stride)
    (if (<= low high)
        (cons low (sequence (+ low stride) high stride))
        null
        )))

(define string-append-map
  (lambda (xs suffix)
    (map (lambda (x) (string-append x suffix)) xs)))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [ #t (letrec ([i (remainder n (length xs))]
                      [f (lambda (ys j)
                           (if (= j i) (car ys) (f (cdr ys) (+ 1 j))))])
               (f xs 0))]))


(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s i)
                (let ([pr (s)])
                  (if (= n i)
                      null
                      (cons (car pr) (f (cdr pr) (+ i 1))))))])
    (f s 0)))

(define funny-number-stream
  (letrec
      ([f (lambda (x) (cons
                       (if (= (remainder x 5) 0)
                           (* -1 x)
                           x)
                       (lambda () (f (+ x 1)))))])
    (lambda ()(f 1))))

(define dan-then-dog
  (letrec
      ([f (lambda (d) (if (equal? d "dan.jpg")
                          (cons "dog.jpg" (lambda () (f "dog.jpg")))
                          (cons "dan.jpg" (lambda () (f "dan.jpg")))))])
    (lambda ()(f "dog.jpg"))))

(define dan-then-dog-another
  (letrec
      ([f (lambda (t)
            (if t
                (cons "dog.jpg" (lambda () (f #f)))
                (cons "dan.jpg" (lambda () (f #t)))))])
    (lambda () (f #f))))

(define (stream-add-zero s)
  (lambda ()
    (let ([pr (s)])
      (cons
       (cons 0 (car pr))
       (stream-add-zero (cdr pr))))))


(define (cycle-lists xs ys)
  (letrec ([xsi xs]
           [ysi ys]
           [f (lambda (xs ys)
                (let ([g (lambda ()
                           (cons (cons (car xs) (car ys))
                                 (f
                                  (if (null? (cdr xs)) xsi (cdr xs))
                                  (if (null? (cdr ys)) ysi (cdr ys)))))])
                  g))])
    (f xs ys)))

(define (cycle-lists-another xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n)
                            (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda (i)
                (if (= i l)
                    #f
                    (let ([el (vector-ref vec i)])
                      (if (pair? el)
                          (if (equal? (car el) v)
                              el
                              (f (+ i 1)))
                          (f (+ i 1))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cac (make-vector n #f)]
           [pos 0])
    (lambda (v) (let ([prc (vector-assoc v cac)])
                  (if prc
                      prc
                      (let ([pr (assoc v xs)])
                        (if pr
                            (begin (vector-set! cac pos pr)
                                   (set! pos (if (= (- n 1) pos)
                                                 0
                                                 (+ pos 1)))
                                   pr)
                            #f)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([x e1]
              [f (lambda ()
                   (if (< e2 x) (f) #t)
                   )])
       (f))]))