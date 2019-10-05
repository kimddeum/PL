#lang plai

; This is another language, ArithC, that supports only addition (plusC) and multiplication (multC).
; Example code of this language is as follows
; {+ 1 {+ 2 3}}
; {* 3 {* 2 {+ 3 4}}}

; BNF for ArithC
; <ArithC> := <numC>
;             | {+ <ArithC> <ArithC>}
;             | {* <ArithC> <ArithC>}
; 
; Assume we know terminals as follows:
; <numC> can be replaced wtih any numbers.

; Data type for ArithC
(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])

; ========= TASK1: write [contract] and [purpose] for all functions. Check sub tasks, TASK1-1, 1-2, 1-3, and 1-4.

; ===TASK1-1
;[contract]: parse: sexp -> ArithC
;[purpose]: to convert s-expressions into ArithC in abstract syntax
(define (parse s)
  (cond
    [(number? s) (numC s)]
    [(eq? (first s) '+) (plusC (parse (second s)) (parse (third s)))]
    [(eq? (first s) '*) (multC (parse (second s)) (parse (third s)))]
    [else (error 'parse "bad syntax: ~a" s)]))


; BNF for ArithS which supports binary subtration as well
; <ArithS> := <numC>
;             | {+ <ArithS> <ArithS>}
;             | {* <ArithS> <ArithS>}
;             | {- <ArithS> <ArithS>}
; 
; Asuume we know terminals as follows:
; <numC> is any numbers.

; Data type for ArithS
(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [multS (l ArithS?) (r ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)])

; ===TASK1-2
;[contract]: parseForArithS: sexp -> ArithS
;[purpose]: to convert s-expressions into ArithS in abstract syntax
(define (parseForArithS s)
  (cond
    [(number? s) (numS s)]
    [(eq? (first s) '+) (plusS (parseForArithS (second s)) (parseForArithS (third s)))]
    [(eq? (first s) '*) (multS (parseForArithS (second s)) (parseForArithS (third s)))]
    [(eq? (first s) '-) (bminusS (parseForArithS (second s)) (parseForArithS (third s)))]
    [else (error 'parse "bad syntax: ~a" s)]))


; ===TASK1-3
;[contract]: desugar: ArithS -> ArithC
;[purpose]: to convert ArithS into ArithC in abstract syntax
; ===TASK2: Implement desugar function. Check this link for your reference. http://cs.brown.edu/courses/cs173/2012/book/first-desugar.html
; If you can successfully implement this, the last three test cases will pass!!
(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))
  

; ===TASK1-4
;[contract]: interp: ArithC -> num
;[purpose]: to convert ArithC into number
(define (interp a)
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

;[tests] for ArithC
(test (interp (parse '{+ 1 {+ 2 3}})) 6)
(test (interp (parse '{* 3 {* 2 {+ 3 4}}})) 42)
(test/exn (interp (parse '{- 1 2})) "parse: bad syntax: (- 1 2)")


;[tests] for ArithS
(test (interp (desugar (parseForArithS '{+ 1 {+ 2 3}}))) 6)
(test (interp (desugar (parseForArithS '{* 3 {* 2 {+ 3 4}}}))) 42)
(test (interp (desugar (parseForArithS '{- 1 2}))) -1)


; TASK3: Asnwer the following questions
; (1) What is syntactic sugar? Explain based on your implementaion.
; Syntatic sugar, as it's name implies, add something syntatically sweet on the program. Based on the desugar function, it does not modify additional syntax on the original function, it just duplicates another function. I guess syntatic sugar might be adding additiona syntax directly on the given function.   
; (2) What is desugaring? Explain based on your implementaion. Hint: NOTE THAT WE DO NOT NEED TO CHANGE CURRENT interp implmentation for HW2 **
; Desugaring, on the other hand, means removing something on the syntax. What I have implemented is to change one data type into another data type without modifying common function, interp. Through desugaring, it can clearly generate new function, that inherits from original function, to perform additional tasks.
