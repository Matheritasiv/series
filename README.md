# series
Formal power series and 10-adic integers in scheme language.

Run with Chez Scheme

## Formal Power Series

Formal power series represented by infinite streams, and implementation of their algebraic operations: **addition**, **subtraction**, **multiplication**, **division**, **composition** and **inversion**. Definitions of several elementary analytic functions are provided.

### Example 1
To calculate the power series expansion of arctangent function, we can use the following code
```scheme
(display-series (series@/ tan-series))
```
It outputs
```
0
1
0
-1/3
0
1/5
0
-1/7
0
1/9
0
-1/11
0
1/13
0
-1/15
0
1/17
0
-1/19
......
```

### Example 2
To calculate the power series expansion at origin of the analytic function `f` satisfying
![image](https://github.com/Matheritasiv/series/raw/main/formula1.svg)
we can use the following code
```scheme
(display-series
  (series/@
    (series/
      (stream-shift
        (series-
          sin-series
          tan-series)
        -2)
      (stream-shift
        (series@
          exp-series
          (series-
            (series@
              (power-series 1/6)
              (series@
                arctan-series
                (series-
                  one-series
                  cos-series)))
            (series@
              cos-series
              sin-series)))
        -2))
    (series-
      (series@
        log-series
        (series-
          cos-series
          one-series))
      arcsin-series)))
```
It outputs
```
0
6/7
-18/49
177/343
-144/343
6573101/12101040
-7346057/14117880
7014764399/13282101504
-2555274621/5165261696
649864795175867/1757222028979200
-289530661529/4271025764880
-13009473194016331829/22731424166874931200
23702542963315856929/13259997430677043200
-41585484063596772648966649/10425540379895518445568000
1328379991759177672918225429/170283826204960134610944000
-38098310307558367803169871371/2681970262728122120122368000
58795903188923685802022592151/2383973566869441884553216000
-265877588065447862772416835048496991/6434153939095274091058365726720000
201827044599639883564812628550695349/3002605171577794575827237339136000
-61223993928909516849024076546042504590221/575058942460579217162432495191326720000
......
```
which means the result is
![image](https://github.com/Matheritasiv/series/raw/main/formula2.svg)

## 10-adic Integers

10-adic Integers represented by infinite streams, and implementation of their arithmetic operations: **addition**, **subtraction**, **multiplication**, **division**, **exponentiation**, **square root** and **rooting**. Definitions of general idempotents in 10-adic ring are provided.

### Example 3
To verify the general idempotent `I2` satisfies the algebraic relation `I2^3 + I2 = 0`，we can use the following code
```scheme
(display-digits (digits+ (digits-expt I2-digits 3) I2-digits))
```
It outputs
```
000000000000000000000000000000000000000000000000000000000000......
```

### Example 4
To calculate the formula
![image](https://github.com/Matheritasiv/series/raw/main/formula3.svg)
we can use the following code
```scheme
(display-digits
  (digits-expt
    (digits/n
      (digits+
        (digits-root
          (digits-
            (digits*
              (n-digits 22018184530132180980079891532388137430603576999923590090053136118820859838122909049429782255969109364731)
              (digits-sqrt (n-digits 927782336561041)))
            (n-digits 728601742633179951715589146890527259469445942290134538241791259897895270650214811846082995419609592150828))
          13)
      (n-digits 28))
    11)
  6))
```
It outputs
```
129191069035144539403060207957342570935616897000000000000000......
```
which means the result is an integer
![image](https://github.com/Matheritasiv/series/raw/main/formula4.svg)

## License
[WTFPL](http://www.wtfpl.net/txt/copying)
