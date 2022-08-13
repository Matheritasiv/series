(define seg 24)
(define digits (vector
    000000000000000000000000
    000000000000000000000001
    045215487480163574218751
    839804103263499879186432
    182803640476581907922943
    137588152996418333704193
    977392256259918212890624
    977392256259918212890625
    022607743740081787109375
    022607743740081787109376
    862411847003581666295807
    817196359523418092077057
    160195896736500120813568
    954784512519836425781249
    999999999999999999999999
))
(define len (vector-length digits))

(define (compare a b)
  (let-values ([(ah at) (div-and-mod a 10)]
               [(bh bt) (div-and-mod b 10)])
    (cond [(< at bt) -1]
          [(> at bt) 1]
          [(and (zero? ah) (zero? bh)) 0]
          [else (compare ah bh)])))

(define (find-index digit)
  (let loop ([a 0] [b len] [c (quotient len 2)])
    (if (= a b) -1
      (case (compare digit (vector-ref digits c))
        [(-1) (loop a c (quotient (+ a c) 2))]
        [(1) (loop (1+ c) b (quotient (+ 1 c b) 2))]
        [else c]))))

(let loop ([i 0] [j 0])
  (when (< i len)
    (if (> j i) (begin (newline) (loop (1+ i) 0))
      (let* ([a (vector-ref digits i)] [b (vector-ref digits j)]
             [k (find-index (remainder (* a b) (expt 10 seg)))])
        (printf "A~2a×A~2a=A~2a  " i j k)
        (loop i (1+ j))))))
