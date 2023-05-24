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
  (stream-for-each display s))
(define (display-line x) (display x) (newline))
(define (display-series s)
  (stream-for-each display-line s))
(define (display-flush x) (display x) (flush-output-port))
(define (display-digits s)
  (stream-for-each display-flush s))
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
    (let loop ([n n])
      (if (zero? n) s
        (stream-cons 0 (loop (1- n)))))))
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
(define (series-square s)
  (let ([s (stream-cons 0 s)])
    (let loop ([k 0])
      (stream-cons
        (let loop ([j (* 2 k)] [s s] [l (list)] [a 0])
          (if (negative? j) a
            (let* ([s (stream-cdr s)] [x (stream-car s)])
              (cond [(> j k) (loop (- j 2) s (cons x l) a)]
                    [(< j k) (loop (- j 2) s (cdr l) (+ a (* 2 (car l) x)))]
                    [else (loop (- j 2) s l (+ a (* x x)))]))))
        (loop (1+ k))))))
(define (series-expt s n)
  (cond [(negative? n) (series-expt (series/ s) (- n))]
        [(zero? n) (stream-cons 1 zeros)] [(= n 1) s]
        [else
         (let ([ss (series-square (series-expt s (quotient n 2)))])
           (if (odd? n) (series* ss s) ss))]))
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

;;{{{ Basic construction of digits
(define zero-digits (ns 0))
(define one-digits (stream-cons 1 zero-digits))
(define minus-one-digits (ns 9))
;;}}}
;;{{{ Basic operations of digits
(define (stream-carry s)
  (let loop ([c 0] [d s])
    (let-values ([(c r)
        (div-and-mod (+ c (stream-car d)) 10)])
      (stream-cons r (loop c (stream-cdr d))))))
(define digits-shift stream-shift)
(define (digits+ . args)
  (stream-carry (apply stream+ args)))
(define digits-
  (case-lambda
    [(d)
     (let loop ([q 1] [d d])
       (let ([a (+ q 9 (- (stream-car d)))])
         (if (> a 9)
           (stream-cons (- a 10) (loop 1 (stream-cdr d)))
           (stream-cons a (loop 0 (stream-cdr d))))))]
    [(d1 d2)
     (digits+ d1 (digits- d2))]))
(define (n-digits n)
  (let loop ([n n])
    (cond [(negative? n) (digits- (loop (- n)))]
          [(zero? n) (ns 0)]
          [else
           (let-values ([(q r) (div-and-mod n 10)])
             (stream-cons r (loop q)))])))
(define (digits*n d n)
  (cond [(negative? n) (digits*n (digits- d) (- n))]
        [(zero? n) zero-digits]
        [else (stream-carry (stream*n d n))]))
(define (digits/2 d)
  (stream-cdr (digits*n d 5)))
(define (digits/5 d)
  (stream-cdr (digits*n d 2)))
(define (digits* d1 d2)
  (let loop ([d1 d1])
    (digits+ (digits*n d2 (stream-car d1))
             (stream-cons 0
               (loop (stream-cdr d1))))))
(define (digits-square d)
  (let loop ([k 0] [q 0])
    (let-values ([(q r) (div-and-mod
        (let loop ([j (* 2 k)] [d d] [l (list)] [a q])
          (if (negative? j) a
            (let ([d (stream-cdr d)] [x (stream-car d)])
              (cond [(> j k) (loop (- j 2) d (cons x l) a)]
                    [(< j k) (loop (- j 2) d (cdr l) (+ a (* 2 (car l) x)))]
                    [else (loop (- j 2) d l (+ a (* x x)))])))) 10)])
      (stream-cons r (loop (1+ k) q)))))
(define (digits-expt d n)
  (cond [(negative? n) (digits-expt (digits/ d) (- n))]
        [(zero? n) one-digits] [(= n 1) d]
        [else
         (let ([dd (digits-square (digits-expt d (quotient n 2)))])
           (if (odd? n) (digits* dd d) dd))]))
(define (digits/n d n)
  (if (negative? n) (digits/n (digits- d) (- n))
    (let ([c (remainder n 10)])
      (case c
        [(1 3 7 9)
         (let ([b (case c [1 1] [3 7] [7 3] [9 9])])
           (let loop ([q 0] [d d])
             (let ([x (mod (* b (- (stream-car d) q)) 10)])
               (stream-cons x
                 (loop (quotient (+ q (* n x)) 10) (stream-cdr d))))))]
        [else
         (error 'digits/n
           "Number is non-invertible.")]))))
(define digits/
  (case-lambda
    [(d)
     (let ([c (stream-car d)])
       (case c
         [(1 3 7 9)
          (let ([b (case c [1 1] [3 7] [7 3] [9 9])])
            (letrec ([inv (stream-cons 0 (stream-cons b
                (let loop ([k 1] [q (quotient (* b c) 10)])
                  (let ([a
                      (let loop ([a q] [d inv] [l
                          (let loop ([j k] [d d] [l (list)])
                            (if (<= j 0) l
                              (let ([d (stream-cdr d)])
                                (loop (1- j) d (cons (stream-car d) l)))))])
                        (if (null? l) a
                          (let* ([d (stream-cdr d)] [x (stream-car d)])
                            (loop (+ a (* x (car l))) d (cdr l)))))])
                    (let ([x (mod (* (remainder a 10) (- b)) 10)])
                      (stream-cons x
                        (loop (1+ k) (quotient (+ a (* c x)) 10))))))))])
              (stream-cdr inv)))]
         [else
          (error 'digits/
            "Digits is non-invertible.")]))]
    [(d1 d2)
     (digits* d1 (digits/ d2))]))
(define digits-sqrt
  (case-lambda
    [(d) (digits-sqrt d 0)]
    [(d index)
     (let ([c (stream-car d)] [d (stream-cdr d)])
       (case c
         [(1 9)
          (if (let ([c (stream-car d)])
                (or (odd? c) (boolean=? (odd? (stream-car (stream-cdr d)))
                                        (even? (quotient c 2)))))
            (error 'digits-sqrt
              "Digits has no square roots.")
            (let* ([c (quotient (if (even? (quotient index 2)) (+ c 3) (- 37 c)) 4)]
                   [b (case c [1 1] [3 7] [7 3] [9 9])] [e (even? index)])
              (letrec ([rt (stream-cons c
                  (let loop ([k 1] [d d] [q (quotient (* c c) 10)])
                    (let-values ([(a s)
                        (let loop ([j (* 2 (1- k))] [r rt] [l (list)] [a q] [s (void)])
                          (if (<= j 0) (values a s)
                            (let* ([r (stream-cdr r)] [x (stream-car r)])
                              (cond [(> j k) (loop (- j 2) r (cons x l) a s)]
                                    [(< j k) (loop (- j 2) r (cdr l) (+ a (* 2 (car l) x))
                                                   (if (= (1+ j) k) (even? x) s))]
                                    [else (loop (- j 2) r l (+ a (* x x)) #t)]))))])
                      (let* ([y (stream-car d)] [x (mod (quotient (* b (- y a)) 2) 5)]
                             [d (stream-cdr d)] [q (quotient (+ a (* 2 c x)) 10)])
                        (let-values ([(x q) (if
                            (or (and (boolean? s) (boolean=? s (boolean=?
                                    (even? (stream-car d)) (even? q))))
                                (and (not (boolean? s)) e))
                            (values x q) (values (+ x 5) (+ q c)))])
                          (stream-cons x (loop (1+ k) d q)))))))])
                rt)))]
         [else
          (error 'digits-sqrt
            "Only digits of 1 or 9 is acceptable.")]))]))
(define (digits-root d n)
  (cond [(negative? n) (digits-root (digits/ d) (- n))]
        [(= n 1) d] [else (case (remainder n 10)
    [(1 3 7 9)
     (let ([c (stream-car d)])
       (case c
         [(1 3 7 9)
          (let ([iter0 (n-digits (if (= (remainder n 4) 1) c
              (case c [1 1] [3 7] [7 3] [9 9])))]
                [next (lambda (x)
              (digits/n (digits+ (digits* d (digits-expt x (- 1 n)))
                                 (digits*n x (1- n))) n))])
            (let oloop ([skip 0] [d1 iter0] [dd1 iter0] [d2 (next iter0)])
              (let loop ([skip skip] [dd1 dd1] [dd2
                  (let loop ([j skip] [d2 d2])
                    (if (<= j 0) d2 (loop (1- j) (stream-cdr d2))))])
                (let ([x (stream-car dd2)])
                  (if (= x (stream-car dd1))
                    (stream-cons x
                      (loop (1+ skip) (stream-cdr dd1) (stream-cdr dd2)))
                    (oloop skip d2 dd2 (next d2)))))))]
         [else
          (error 'digits-root
            "Only invertible digits is acceptable.")]))]
    [else
     (error 'digits-root
       "Only root index ending with 1, 3, 7 or 9 is acceptable.")])]))
;;}}}
;;{{{ Definitions of general idempotents
;;;{{{ Idempotents in 10-adic ring
(define idemv-digits
  (letrec ([r (stream-cons 5
      (let loop ([k 1] [q 2])
        (let-values ([(q r) (div-and-mod
            (let loop ([j (* 2 (1- k))] [r r] [l (list)] [a q])
              (if (<= j 0) a
                (let* ([r (stream-cdr r)] [x (stream-car r)])
                  (cond [(> j k) (loop (- j 2) r (cons x l) a)]
                        [(< j k) (loop (- j 2) r (cdr l) (+ a (* 2 (car l) x)))]
                        [else (loop (- j 2) r l (+ a (* x x)))])))) 10)])
          (stream-cons r (loop (1+ k) (+ q r))))))])
    r))
(define idemvi-digits (digits- one-digits idemv-digits))
;;;}}}
;;; I0 = I1 - 1 = I9' + 1 = 0
(define I0-digits zero-digits)
(define I1-digits one-digits)
(define I9--digits minus-one-digits)
;;; I1'^2 - 1 = I9^2 - 1 = 0
(define I1--digits (digits-sqrt one-digits 1))
(define I9-digits (digits- I1--digits))
;;; I3^4 - 1 = I3'^4 - 1 = I7^4 - 1 = I7'^4 - 1 = 0
(define I3-digits (digits-sqrt I9-digits))
(define I7--digits (digits- I3-digits))
(define I3--digits (digits-sqrt I9-digits 1))
(define I7-digits (digits- I3--digits))
;;; I4^2 + I4 = I5^2 - I5 = I5'^2 + I5' = I6^2 - I6 = 0
(define I5--digits (digits/2 I1--digits))
(define I6-digits (digits+ one-digits I5--digits))
(define I4-digits (digits- I6-digits))
(define I5-digits (digits- I5--digits))
;;; I2^3 + I2 = I8^3 + I8 = 0
(define I8-digits (digits+ I5--digits I3--digits))
(define I2-digits (digits- I8-digits))
;;}}}

;;{{{ Definitions of some sequences
;;;{{{ Prime numbers (https://oeis.org/A000040)
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
;;;}}}
;;;{{{ Partition numbers (https://oeis.org/A000041)
(define partition
  (series/
    (let loop ([b 1] [st 0] [cnt 0])
      (if (zero? st)
        (stream-cons b
          (loop (- b) (1+ (* 3 cnt)) (1+ cnt)))
        (stream-cons (if (= st cnt) b 0)
          (loop b (1- st) cnt))))))
;;;}}}
;;;{{{ Integer part of the sequence (sqrt(5)+1)/2*n (https://oeis.org/A000201)
(define int-part-of-n*phi
  (letrec ([A (stream-cons 1 (stream-cons 3
      (let loop ([B (stream-cdr (stream+ A integers))] [n 4])
        (if (< n (stream-car B))
          (stream-cons n (loop B (1+ n)))
          (loop (stream-cdr B) (1+ n))))))])
    A))
;;;}}}
;;;{{{ Tree4 numbers (https://oeis.org/A000602)
(define tree4
  (letrec* ([r (stream-cons 1
      (series*n
        (series+
          (series* r
            (series+ (series*n (series@az^n r 1 2) 3)
                     (series-square r)))
          (series*n (series@az^n r 1 3) 2))
        1/6))]
      [r2 (series-square r)] [r@2 (series@az^n r 1 2)])
    (series+ (series*n r2 -1/2) (series*n r@2 1/2) r
      (stream-shift
        (series*n
          (series+
            (series* r2
              (series+ r2
                (series*n r@2 6)))
            (series*n (series@az^n r2 1 2) 3)
            (series*n (series* r (series@az^n r 1 3)) 8)
            (series*n (series@az^n r 1 4) 6))
          1/24) 1))))
;;;}}}
;;;{{{ Bernoulli sequence (https://oeis.org/A027641, https://oeis.org/A027642)
(define Bernoullis
  (stream/
    (series/ (stream-shift exp-series -1))
    exp-series))
;;;}}}
;;;{{{ The non-trivial square root of 1 in 10-adic ring (https://oeis.org/A063006)
#;(define sqrt-of-one-digits
  (digits- one-digits (digits*n idemv-digits 2)))
(define sqrt-of-one-digits
  (letrec ([r (stream-cons 1 (stream-cons 5
      (let loop ([k 2] [d 1])
        (let-values ([(a s)
            (let loop ([j (* 2 (1- k))] [r r] [l (list)] [a d] [s #f])
              (if (<= j 0) (values a s)
                (let* ([r (stream-cdr r)] [x (stream-car r)])
                  (cond [(> j k) (loop (- j 2) r (cons x l) a s)]
                        [(< j k) (loop (- j 2) r (cdr l) (+ a (* 2 (car l) x))
                                       (if (= (1+ j) k) (odd? x) s))]
                        [else (loop (- j 2) r l (+ a (* x x)) s)]))))])
          (let* ([d (ceiling (/ a 10))]
                 [d ((if (boolean=? (odd? d) s) values 1+) d)])
            (stream-cons (quotient (- (* 10 d) a) 2) (loop (1+ k) d)))))))])
    r))

(define sqrt-of-one-digits-newton-method-1
  (let ([iter0 (n-digits 11)] [next (lambda (d)
      (digits/2 (digits+ d (digits/ d))))])
    (let oloop ([skip 0] [d1 iter0] [dd1 iter0] [d2 (next iter0)])
      (let loop ([skip skip] [dd1 dd1] [dd2
          (let loop ([j skip] [d2 d2])
            (if (<= j 0) d2 (loop (1- j) (stream-cdr d2))))])
        (let ([x (stream-car dd2)])
          (if (= x (stream-car dd1))
            (stream-cons x
              (loop (1+ skip) (stream-cdr dd1) (stream-cdr dd2)))
            (oloop skip d2 dd2 (next d2))))))))

(define sqrt-of-one-digits-newton-method-2
  (let* ([next (lambda (x) (/ (+ x (/ x)) 2))]
         [r-digits (lambda (r)
                     (digits/n (n-digits (numerator r))
                               (denominator r)))]
         [x0 11] [iter0 (r-digits x0)] [x1 (next x0)])
    (let oloop ([x (next x1)] [skip 0] [d1 iter0] [dd1 iter0] [d2 (r-digits x1)])
      (let loop ([skip skip] [dd1 dd1] [dd2
          (let loop ([j skip] [d2 d2])
            (if (<= j 0) d2 (loop (1- j) (stream-cdr d2))))])
        (let ([c (stream-car dd2)])
          (if (= c (stream-car dd1))
            (stream-cons c
              (loop (1+ skip) (stream-cdr dd1) (stream-cdr dd2)))
            (oloop (next x) skip d2 dd2 (r-digits x))))))))
;}}}
;;}}}

(display-series Bernoullis)
(display-series (series@/ arctan-series))
;(display-digits (digits+ (digits-expt I2-digits 3) I2-digits))
