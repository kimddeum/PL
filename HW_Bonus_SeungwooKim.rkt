#lang plai ;Solved by myself: Y,but I shared testcases with my colleagues whether mine and other's testcases are right or not, time taken: 7 hours

;; What was the most difficult part when integrating BMFAE and RCFAE?

; Updating interp function that supports if0 and rec branch. It keep alerts store-lookup: No value at address error. I spent lots of time debugging this error.

;; How could you solve the most difficult part in which way. (Explain the algorithm)

; Since the problem is there is no Store saved when app is called, I put the case on store-lookup function that if the address is an integer, then perform as it before
; and if address is not an integer, it means there is no store saved for it. In other words, if statement in store-lookup function indicates that it is used for recusive function.

; type definition for abstract syntax tree of BMRCFAE
(define-type BMRCFAE
  [num (n number?)]
  [add (lhs BMRCFAE?) (rhs BMRCFAE?)]
  [sub (lhs BMRCFAE?) (rhs BMRCFAE?)]
  [mul (lhs BMRCFAE?) (rhs BMRCFAE?)]
  [id (name symbol?)]
  [fun (param prm?) (body BMRCFAE?)]
  [setvar (name symbol?) (v BMRCFAE?)]
  [newbox  (v BMRCFAE?)]
  [setbox  (bn BMRCFAE?) (v BMRCFAE?)]
  [openbox  (v BMRCFAE?)]
  [seqn  (ex1 BMRCFAE?) (ex2 BMRCFAE?)]
  [app     (ftn BMRCFAE?) (arg BMRCFAE?)]
  [prm    (calltype symbol?) (param symbol?)]
  [if0 (test-expr BMRCFAE?) (then-expr BMRCFAE?) (else-expr BMRCFAE?)]
  [rec (name symbol?) (named-expr BMRCFAE?) (fst-call BMRCFAE?)]
  )
        
; parse : sexp -> BMRCFAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun (prm 'val i) (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'newbox v) (newbox (parse v))]
    [(list 'setbox i v) (setbox (parse i) (parse v))]
    [(list 'openbox i) (openbox (parse i))]
    [(list 'seqn ex1 ex2) (seqn (parse ex1) (parse ex2))]
    [(list 'fun p b) (fun (parse p) (parse b))]
    [(list 'ref p) (prm 'ref p)]
    [(list 'val p) (prm 'val p)]
    [(list f 'ref p) (prm (parse f) 'ref p)]
    [(list f 'val p) (prm (parse f) 'val p)]
    [(list f a) (app (parse f) (parse a))]
    [(list 'setvar n v) (setvar n (parse v))]
    [(list 'if0 test then else) (if0 (parse test) (parse then) (parse else))]
    [(list 'rec (list n b) c) (rec n (parse b) (parse c))]
    [else                       (error 'parse "bad syntax: ~a" sexp)]))

(define-type BMRCFAE-Value
  [numV (n number?)]
  [closureV (param prm?) (body BMRCFAE?) (ds DefrdSub?)]
  [exprV (expr BMRCFAE?) (ds DefrdSub?) (value (box/c (or/c false BMRCFAE-Value?)))]
  [boxV      (address integer?)])

(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)]
  [aRecSub (name symbol?) (value-box (box/c Value*Store?)) (ds DefrdSub?)])

(define-type Store
  [mtSto]
  [aSto (address integer?) (value BMRCFAE-Value?) (rest Store?)])

(define-type Value*Store
  [v*s (value BMRCFAE-Value?) (store Store?)])

;lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i adr saved) (if(symbol=? i name) adr (lookup name saved))]
    [aRecSub (i val-box rest-ds) (if (symbol=? i name)
                                     (unbox val-box)
                                     (lookup name rest-ds))]))

;store-lookup address Store -> BMRCFAE-Value
(define (store-lookup address sto)
  (if (integer? address)
      (type-case Store sto
        [mtSto () (error 'store-lookup "No value at address")]
        [aSto  (location value rest-store)
               (if(= location address)
                  value
                  (store-lookup address rest-store))])
      (type-case Value*Store address
        [v*s (v s) v])))

; malloc: Store -> Integer
(define (malloc st)
  (+ 1 (max-address st)))

; max-address: Store -> Integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

; numzero?: Value*Store -> boolean
(define (numzero? n)
  (type-case Value*Store n
    [v*s (v s) (zero? (numV-n v))]))
  
; interp: BMRCFAE DefrdSub -> Value*Store
(define (interp bmrcfae ds st)
  (type-case BMRCFAE bmrcfae
    [num (n) (v*s (numV n) st)]
    [add (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    [mul (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num* v1 v2) st1)))]
    [id (s) (v*s (store-lookup (lookup s ds) st) st)]
    [fun (p b) (v*s (closureV p b ds) st)]
    [prm    (ct p) 0]
    [app (f a) (type-case Value*Store (interp f ds st)
                 [v*s (f-value f-store)
                      (type-case BMRCFAE-Value f-value
                        [closureV (c-param c-body c-ds)
                                  (type-case Value*Store (interp a ds f-store)
                                    [v*s (a-value a-store)
                                         (local [(define new-address
                                                   (if (eq? 'val (prm-calltype (closureV-param f-value)))
                                                       (malloc a-store)
                                                       (lookup (id-name a) ds)))]
                                           (interp c-body 
                                                   (aSub (prm-param c-param) new-address c-ds)
                                                   (aSto new-address a-value a-store)))])]
                        
                        [else (error 'interp "numV case")])])]
    
    [newbox (val) (type-case Value*Store (interp val ds st)
                    [v*s (vl st1)
                         (local [(define a (malloc st1))]
                           (v*s (boxV a)
                                (aSto a vl st1)))])]
    [setbox (bx-expr val-expr)
            (interp-two bx-expr val-expr ds st
                        (lambda (bx-val val st1)
                          (v*s val
                               (aSto (boxV-address bx-val)
                                     val
                                     st1))))]
    [openbox (bx-expr)
             (type-case Value*Store (interp bx-expr ds st)
               [v*s (bx-val st1)
                    (v*s (store-lookup (boxV-address bx-val)
                                       st1)
                         st1)])]
    [seqn (a b) (interp-two a b ds st (lambda (v1 v2 st1) (v*s v2 st1)))]
    [setvar (id val-expr) (local [(define a (lookup id ds))]
                           (type-case Value*Store (interp val-expr ds st)
                             [v*s (val st)
                                  (v*s val
                                       (aSto a val st))]))]
    [if0 (test-expr then-expr else-expr)
         (if (numzero? (interp test-expr ds st))
             (interp then-expr ds st)
             (interp else-expr ds st))]
    [rec (name named-expr fst-call)
      (local [(define value-holder (box(v*s (numV 12) (mtSto))))
              (define new-ds (aRecSub name value-holder ds))]
      (begin
        (set-box! value-holder (interp named-expr new-ds st))
        (interp fst-call new-ds st)))]
    ))

;interp-two: BMRCFAE BMRCFAE DefrdSub Store
;            (Value Value Store -> Value*Store)
;            -> Value*Store
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         [type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)]]]))

(define (run sexp ds st)
     (interp (parse sexp) ds st))

;; Recursive part
;(parse '{rec {count {fun {val n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}})
;(interp (rec 'count (fun (prm 'val 'n) (if0 (id 'n) (num 0) (add (num 1) (app (id 'count) (sub (id 'n) (num 1)))))) (app (id 'count) (num 8))) (mtSub) (mtSto))
(test (run '{rec {count {fun {val n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}} (mtSub) (mtSto)) (v*s (numV 8) (aSto 9 (numV 0) (aSto 8 (numV 1) (aSto 7 (numV 2) (aSto 6 (numV 3) (aSto 5 (numV 4) (aSto 4 (numV 5) (aSto 3 (numV 6) (aSto 2 (numV 7) (aSto 1 (numV 8) (mtSto))))))))))))
(test (run '{rec {fac {fun {val n} {if0 n 1 {* n {fac {- n 1}}}}}} {fac 10}} (mtSub) (mtSto)) (v*s (numV 3628800) (aSto 11 (numV 0) (aSto 10 (numV 1) (aSto 9 (numV 2) (aSto 8 (numV 3) (aSto 7 (numV 4) (aSto 6 (numV 5) (aSto 5 (numV 6) (aSto 4 (numV 7) (aSto 3 (numV 8) (aSto 2 (numV 9) (aSto 1 (numV 10) (mtSto))))))))))))))

(test (run '{{fun {val a} a} 10} (mtSub) (mtSto)) (v*s (numV 10) (aSto 1 (numV 10) (mtSto))))
(test (run '{fun {ref a} a} (mtSub) (mtSto)) (v*s (closureV (prm 'ref 'a) (id 'a) (mtSub)) (mtSto)))
(test (run '{with {x 10} {{fun {ref a} a} x}} (mtSub) (mtSto)) (v*s (numV 10) (aSto 1 (numV 10) (aSto 1 (numV 10) (mtSto)))))
(test (run '{with {x 5} x} (mtSub) (mtSto)) (v*s (numV 5) (aSto 1 (numV 5) (mtSto))))
(test (run '{with {a 3} {seqn {{fun {val x} {setvar x 5}} a} a}}(mtSub) (mtSto)) (v*s (numV 3) (aSto 2 (numV 5) (aSto 2 (numV 3) (aSto 1 (numV 3) (mtSto))))))
(test (run '{with {a 3} {seqn {{fun {ref x} {setvar x 5}} a} a}}(mtSub) (mtSto)) (v*s (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (aSto 1 (numV 3) (mtSto))))))

(test (run '{with {a 3} {seqn {{fun {ref x} {setvar x 5}} a}
                             {rec {count {fun {val z} {if0 z 0 {+ 1 {count{- z 1}}}}}}{count a}}}}(mtSub) (mtSto))
             (v*s (numV 5) (aSto 7 (numV 0) (aSto 6 (numV 1) (aSto 5 (numV 2) (aSto 4 (numV 3) (aSto 3 (numV 4) (aSto 2 (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (aSto 1 (numV 3) (mtSto))))))))))))

(test (run '{with {a 3} {seqn {{fun {val x} {setvar x 5}} a}
                             {rec {count {fun {val z} {if0 z 0 {+ 1 {count{- z 1}}}}}}{count a}}}}(mtSub) (mtSto))
             (v*s (numV 3) (aSto 6 (numV 0) (aSto 5 (numV 1) (aSto 4 (numV 2) (aSto 3 (numV 3) (aSto 2 (numV 5) (aSto 2 (numV 3) (aSto 1 (numV 3) (mtSto))))))))))

;; Call-by-Value, Call-by-Reference part
(test (parse '{{fun {val a} a} 10}) (app (fun (prm 'val 'a) (id 'a)) (num 10)))
(test (parse '{fun {ref a} a}) (fun (prm 'ref 'a) (id 'a)))
(test (parse '{with {x 10} {{fun {ref a} a} x}}) (app (fun (prm 'val 'x) (app (fun (prm 'ref 'a) (id 'a)) (id 'x))) (num 10)))
(test (parse '{with {x 5} x}) (app (fun (prm 'val 'x) (id 'x)) (num 5)))
(test (parse '{with {a 3} {seqn {{fun {val x} {setvar x 5}} a} a}}) (app (fun (prm 'val 'a) (seqn (app (fun (prm 'val 'x) (setvar 'x (num 5))) (id 'a)) (id 'a))) (num 3)))
(test (parse '{with {a 3} {seqn {{fun {ref x} {setvar x 5}} a} a}}) (app (fun (prm 'val 'a) (seqn (app (fun (prm 'ref 'x) (setvar 'x (num 5))) (id 'a)) (id 'a))) (num 3)))

(test (run '{{fun {val a} a} 10} (mtSub) (mtSto)) (v*s (numV 10) (aSto 1 (numV 10) (mtSto))))
(test (run '{fun {ref a} a} (mtSub) (mtSto)) (v*s (closureV (prm 'ref 'a) (id 'a) (mtSub)) (mtSto)))
(test (run '{with {x 10} {{fun {ref a} a} x}} (mtSub) (mtSto)) (v*s (numV 10) (aSto 1 (numV 10) (aSto 1 (numV 10) (mtSto)))))
(test (run '{with {x 5} x} (mtSub) (mtSto)) (v*s (numV 5) (aSto 1 (numV 5) (mtSto))))
(test (run '{with {a 3} {seqn {{fun {val x} {setvar x 5}} a} a}}(mtSub) (mtSto)) (v*s (numV 3) (aSto 2 (numV 5) (aSto 2 (numV 3) (aSto 1 (numV 3) (mtSto))))))
(test (run '{with {a 3} {seqn {{fun {ref x} {setvar x 5}} a} a}}(mtSub) (mtSto)) (v*s (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (aSto 1 (numV 3) (mtSto))))))
     

(test (run '{with {swap {fun {ref x}
                       {fun {ref y}
                            {with {z x}
                                  {seqn {setvar x y}
                                        {setvar y z}}}}}}
            {with {a 10}
                  {with {b 20}
                        {seqn {{swap a} b}
                              a}}}} (mtSub) (mtSto))

      (v*s (numV 20) (aSto 3 (numV 10) (aSto 2 (numV 20) (aSto 4 (numV 10) (aSto 3 (numV 20) (aSto 2 (numV 10) (aSto 3 (numV 20) (aSto 2 (numV 10) (aSto 1 (closureV (prm 'ref 'x) (fun (prm 'ref 'y) (app (fun (prm 'val 'z) (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))) (mtSub)) (mtSto)))))))))))

;;;;;;
;; Swap using fun with call-by-reference for 'b' (not wokring correctly)
;;;;;;
(test (run '{with {swap {fun {ref x}
                       {fun {ref y}
                            {with {z x} ;;; z will be processed as call-by-reference.
                                  {seqn {setvar x y}
                                        {setvar y z}}}}}}
            {with {a 10}
                  {with {b 20}
                        {seqn {{swap a} b}
                              b}}}} (mtSub) (mtSto))
      (v*s (numV 10) (aSto 3 (numV 10) (aSto 2 (numV 20) (aSto 4 (numV 10) (aSto 3 (numV 20) (aSto 2 (numV 10) (aSto 3 (numV 20) (aSto 2 (numV 10) (aSto 1 (closureV (prm 'ref 'x) (fun (prm 'ref 'y) (app (fun (prm 'val 'z) (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))) (mtSub)) (mtSto)))))))))))

;; *****This test case must generate contract violation****
(test (run '{{fun {ref a} a} 10} (mtSub) (mtSto)) (v*s (numV 10) (aSto 1 (numV 10) (mtSto))))