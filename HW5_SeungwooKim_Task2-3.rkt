#lang plai

; type definition for abstract syntax tree of BMFAE
(define-type BMFAE
    [num    (n number?)]
    [add     (lhs BMFAE?) (rhs BMFAE?)]
    [sub     (lhs BMFAE?) (rhs BMFAE?)]
    [id      (name symbol?)]
    [fun      (param prm?) (body BMFAE?)] 
    [setvar     (name symbol?) (v BMFAE?)]
    [newbox  (v BMFAE?)]
    [setbox  (bn BMFAE?) (v BMFAE?)]
    [openbox  (v BMFAE?)]
    [seqn  (ex1 BMFAE?) (ex2 BMFAE?)]
    [app     (ftn BMFAE?) (arg BMFAE?)]
    [prm    (calltype symbol?) (param symbol?)]
  )
        
; parse : sexp -> BMFAE
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun (prm 'val i) (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'newbox v)           (newbox (parse v))]
        [(list 'setbox i v)         (setbox (parse i) (parse v))]
        [(list 'openbox i)          (openbox (parse i))]
        [(list 'seqn ex1 ex2)       (seqn (parse ex1) (parse ex2))]
        [(list 'fun p b)     (fun (parse p) (parse b))]
        [(list 'ref p)              (prm 'ref p)]
        [(list 'val p)              (prm 'val p)]
        [(list f a)                 (app (parse f) (parse a))]
        [(list 'setvar n v)            (setvar n (parse v))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

;(require racket/trace)
;(trace parse)


(define-type BMFAE-Value
  [numV      (n number?)]
  [closureV  (param prm?) (body BMFAE?) (ds DefrdSub?)]
  [boxV      (address integer?)])

(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))


(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

(define-type Store
  [mtSto]
  [aSto   (address integer?) (value BMFAE-Value?)
          (rest Store?)])

(define-type Value*Store
  [v*s (value BMFAE-Value?) (store Store?)])

;lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i adr saved) (if(symbol=? i name)
                                adr
                                (lookup name saved))]))

;store-lookup address Store -> BMFAE-Value
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto ()           (error 'store-lookup "No value at address")]
    [aSto  (location value rest-store)
                 (if(= location address)
                     value
                    (store-lookup address rest-store))]))

; malloc: Store -> Integer
(define (malloc st)
  (+ 1 (max-address st)))

; max-address: Store -> Integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

; interp: BMFAE DefrdSub -> Value*Store
(define (interp bmfae ds st)
  (type-case BMFAE bmfae
    [num    (n)    (v*s (numV n) st)]
    [add    (l r)  (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub    (l r)  (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    [id     (s)    (v*s (store-lookup (lookup s ds) st) st)]
    [fun    (p b)  (v*s (closureV p b ds) st)]
    [prm    (ct p) 0]
    [app (f a)       (type-case Value*Store (interp f ds st)
                        [v*s (f-value f-store)
                          (type-case Value*Store (interp a ds f-store)
                              [v*s (a-value a-store)
                                   (local ([define new-address (if (eq? 'val (prm-calltype (closureV-param f-value))) (malloc a-store) (lookup (id-name a) ds))])
                                     (interp (closureV-body f-value)
                                             (aSub (prm-param (closureV-param f-value))
                                                   new-address
                                                   (closureV-ds f-value))
                                             (aSto new-address
                                                   a-value
                                                   a-store)))])])]
    [newbox (val)    (type-case Value*Store (interp val ds st)
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
    ))

;interp-two: BMFAE BMFAE DefrdSub Store
;            (Value Value Store -> Value*Store)
;            -> Value*Store
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         [type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)]]]))

;(run '7 (mtSub) (mtSto))
;(run '{+ 7 6} (mtSub) (mtSto))
;(run '{newbox {+ 2 3}} (mtSub) (mtSto))
;(run '{with {b {newbox {+ 2 3}}} {openbox b}} (mtSub) (mtSto))
;(run '{with {b {newbox 7}} {openbox b}} (mtSub) (mtSto))
;(run '{with {b {newbox 7}}
;         {seqn {setbox b 10}
;                   {openbox b}}} (mtSub) (mtSto))

;(run '{with {a 3} {setvar a 5}} (mtSub) (mtSto))
;(run '{with {a 3} {seqn {{fun {x} {setvar x 5}} a} a}} (mtSub) (mtSto))

;;;;;;
;; Swap using fun with call-by-reference for 'a'
;;;;;;

(test (parse '{{fun {val a} a} 10}) (app (fun (prm 'val 'a) (id 'a)) (num 10)))
(test (parse '{fun {ref a} a}) (fun (prm 'ref 'a) (id 'a)))
(test (parse '{with {x 10} {{fun {ref a} a} x}}) (app (fun (prm 'val 'x) (app (fun (prm 'ref 'a) (id 'a)) (id 'x))) (num 10)))
(test (parse '{with {x 5} x}) (app (fun (prm 'val 'x) (id 'x)) (num 5)))
(test (parse '{with {a 3} {seqn {{fun {val x} {setvar x 5}} a} a}}) (app (fun (prm 'val 'a) (seqn (app (fun (prm 'val 'x) (setvar 'x (num 5))) (id 'a)) (id 'a))) (num 3)))
(test (parse '{with {a 3} {seqn {{fun {ref x} {setvar x 5}} a} a}}) (app (fun (prm 'val 'a) (seqn (app (fun (prm 'ref 'x) (setvar 'x (num 5))) (id 'a)) (id 'a))) (num 3)))

(define (run sexp ds st)
     (interp (parse sexp) ds st))

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