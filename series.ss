;;{{{ Basic construction of stream
(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b)
     (cons a (delay b))]))
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))
(define the-empty-stream '())
(define (stream-null? stream)
  (null? stream))
;;}}}
;;{{{ Basic operation of stream
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc . ss)
  (if (stream-null? (car ss))
    the-empty-stream
    (stream-cons (apply proc (map stream-car ss))
                 (apply stream-map
                        (cons proc (map stream-cdr ss))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter
                        pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (display x) (newline))
;;}}}
;;{{{ Arithmetic operations of stream
(define (streamfy op)
  (lambda args
    (apply stream-map
           (cons op args))))
(define stream+ (streamfy +))
(define stream- (streamfy -))
(define stream* (streamfy *))
(define stream/ (streamfy /))
(define (stream*n s n)
  (stream-map (lambda (x) (* x n)) s))
(define (stream-shift s n)
  (if (negative? n)
    (let loop ([n n] [s s])
      (if (zero? n) s
        (loop (1+ n) (stream-cdr s))))
    (let loop ([n n] [s s])
      (if (zero? n) s
        (loop (1- n) (stream-cons 0 s))))))
;;}}}
;;{{{ Basic construction of series
(define (ns n)
  (letrec ([r (stream-cons n r)]) r))
(define zeros (ns 0))
(define ones (ns 1))
(define integers
  (stream-cons 1
    (stream+ ones integers)))
;;}}}
;;{{{ Basic operations of series
(define (series-integrate s)
  (stream* s (stream-map / integers)))
(define series-derivate
  (case-lambda
    [(s)
     (stream* (stream-shift s -1) integers)]
    [(s n)
     (let loop ([n (1- n)] [s s])
       (if (negative? n) s
         (loop (1- n) (series-derivate s))))]))
(define series+ stream+)
(define series- stream-)
(define series*n stream*n)
(define (series* s1 s2)
  (let loop ([s1 s1])
    (stream+ (stream*n s2 (stream-car s1))
             (stream-cons 0
               (loop (stream-cdr s1))))))
(define series/
  (case-lambda
    [(s)
     (if (zero? (stream-car s))
       (error 'series/
         "Series must not vanish at zero."))
     (letrec ([r
         (stream*n
           (stream-cons 1
             (series-
               (series* (stream-cdr s) r)))
           (/ (stream-car s)))])
       r)]
    [(s1 s2)
     (series* s1 (series/ s2))]))

#;(define (tseries@ s1 s2)
  (let loop ([s1 s1])
    (series* (stream-cons (stream-car s1)
                          (loop (stream-cdr s1)))
             s2)))
(define (tseries@ s1 s2)
  (letrec* ([s1l (list (stream-car s1))]
            [s2l (list (stream-car s2))]
            [ss (stream-cons (list (car s2l))
      (let loop ([s1 (stream-cdr s1)]
                 [s2 (stream-cdr s2)]
                 [n 2] [nfact 2])
        (stream-cons
          (let ([v (make-list n 0)])
            (let loop ([l s1l])
              (set-car! l (/ (car l) n))
              (if (null? (cdr l))
                (set-cdr! l (list (stream-car s1)))
                (loop (cdr l))))
            (let loop ([l s2l] [n (1- n)] [k 1])
              (set-car! l (/ (* (car l) n) k))
              (unless (null? (cdr l))
                (loop (cdr l) n (1+ k))))
            (set! s2l (cons (* nfact (stream-car s2)) s2l))
            (set-car! v (car s2l))
            (let loop ([l (cdr s2l)] [s ss] [k 1])
              (let ([c (car l)] [s (stream-car s)])
                (let loop ([j k] [v (cdr v)] [s s])
                  (when (positive? j)
                    (set-car! v (+ (car v) (* c (car s))))
                    (loop (1- j) (cdr v) (cdr s)))))
              (unless (null? (cdr l))
                (loop (cdr l) (stream-cdr s) (1+ k))))
            v)
          (loop (stream-cdr s1) (stream-cdr s2)
                (1+ n) (* nfact (1+ n))))))])
    (let loop ([ss ss])
      (let ([sl (stream-car ss)])
        (stream-cons (apply + (map * s1l sl))
                     (loop (stream-cdr ss)))))))
(define (tseries^-1 s)
  (if (zero? (stream-car s))
    (error 'tseries^-1
      "The first derivative of series must not vanish at zero."))
  (letrec ([r
      (stream*n
        (stream-cons 1
          (series-
            (series*
              (tseries@ (stream-cdr s) r)
              r)))
        (/ (stream-car s)))])
    r))
(define (series@az^n s a n)
  (stream-cons (stream-car s)
    (let ([n (1- n)])
      (if (negative? n) zeros
        (let loop ([i n] [s (stream-cdr s)] [c a])
          (if (zero? i)
            (stream-cons (* c (stream-car s))
                         (loop n (stream-cdr s) (* c a)))
            (stream-cons 0 (loop (1- i) s c))))))))
(define (series@ s1 s2)
  (if (zero? (stream-car s2))
    (stream-cons (stream-car s1)
                 (tseries@
                   (stream-cdr s1)
                   (stream-cdr s2)))
    (error 'series@
      "Series must vanish at zero.")))
(define series@/
  (case-lambda
    [(s)
     (if (zero? (stream-car s))
       (stream-cons 0 (tseries^-1 (stream-cdr s)))
       (error 'series@/
         "Series must vanish at zero."))]
    [(s1 s2) (series@ s1 (series@/ s2))]))
(define series/@
  (case-lambda
    [(s) (series@/ s)]
    [(s1 s2) (series@ (series@/ s2) s1)]))
;;}}}
;;{{{ Definitions of elementary analytic functions
(define zero-series zeros)
(define 1/<1-z>-series ones)
(define one-series (stream-cons 1 zeros))
(define identity-series (stream-shift one-series 1))
(define square-series   (stream-shift one-series 2))
(define cube-series     (stream-shift one-series 3))
(define quartic-series  (stream-shift one-series 4))
(define quintic-series  (stream-shift one-series 5))
(define exp-series
  (stream-cons 1 (series-integrate exp-series)))
(define log-series
  (stream-cons 0
    (series-integrate
      (series@az^n 1/<1-z>-series -1 1))))
(define (power-series n)
  (letrec ([r
      (stream-cons 1
        (series-integrate
          (series* (series*n r n)
                   (series@az^n 1/<1-z>-series -1 1))))])
    r))
(define cos-series
  (stream-cons 1 (series-integrate (series- sin-series))))
(define sin-series
  (stream-cons 0 (series-integrate cos-series)))
(define tan-series
  (series/ sin-series cos-series))
(define arctan-series
  (stream-cons 0
    (series-integrate
      (series@az^n 1/<1-z>-series -1 2))))
(define arcsin-series
  (stream-cons 0
    (series-integrate
      (series@az^n (power-series -1/2) -1 2))))
;;}}}
;;{{{ Definitions of some sequences
(define Bernoullis
  (stream/
    (series/ (stream-shift exp-series -1))
    exp-series))

(define primes
  (stream-cons 2
    (stream-filter
      (lambda (n)
        (let ([m (isqrt n)])
          (let loop ([ps primes])
            (cond [(> (stream-car ps) m) #t]
                  [(zero? (remainder n (stream-car ps))) #f]
                  [else (loop (stream-cdr ps))]))))
      (stream-shift integers -2))))

(define int-part-of-n*phi
  (letrec ([A (stream-cons 1 (stream-cons 3
      (let loop ([B (stream-cdr (stream+ A integers))] [n 4])
        (if (< n (stream-car B))
          (stream-cons n (loop B (1+ n)))
          (loop (stream-cdr B) (1+ n))))))])
    A))
;;}}}

(display-stream (series@/ tan-series))
