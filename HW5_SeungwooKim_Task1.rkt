#lang plai

(define-type BMFAE
  [num (n number?)]
  [add (lhs BMFAE?) (rhs BMFAE?)]
  [sub (lhs BMFAE?) (rhs BMFAE?)]
  [mul (lhs BMFAE?) (rhs BMFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BMFAE?)]
  [refun (param symbol?) (body BMFAE?)]
  [app (fun BMFAE?) (arg BMFAE?)]
  [newbox (v BMFAE?)]
  [setbox (bn BMFAE?) (v BMFAE?)]
  [openbox (v BMFAE?)]
  [seqn (ex1 BMFAE?) (ex2 BMFAE?)]
  [setvar (name symbol?) (body BMFAE?)])

(define-type BMFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BMFAE?) (ds DefrdSub?)]
  [refclosV (param symbol?) (body BMFAE?) (ds DefrdSub?)]
  [boxV (address integer?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

(define-type Store
  [mtSto]
  [aSto (address integer?) (value BMFAE-Value?) (rest Store?)])

(define-type Value*Store
  [v*s (value BMFAE-Value?) (store Store?)])

; [contract] num-op: (number number -> number) -> (RCFAE RCFAE -> RCFAE)
; [purpose] to calculate the numbers with corresponding operation
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))

; Solved by myself: Yes
; Time taken: less than 5 minutes
; [contract] parse: s-expr -> RMFAE 
; [purpose] to convert sexp to RMFAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list 'refun (list p) b) (refun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'newbox v) (newbox (parse v))]
    [(list 'setbox i v) (setbox (parse i) (parse v))]
    [(list 'openbox i) (openbox (parse i))]
    [(list 'seqn ex1 ex2) (seqn (parse ex1) (parse ex2))]
    [(list 'setvar n v) (setvar n (parse v))]
    [else (error 'parse "bad syntax: ~a" sexp)]))

; [contract] lookup: symbol DefrdSub -> address
; [purpose] get the address of symbol on DefrdSub
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (i addr saved)
          (if (symbol=? i name)
              addr
              (lookup name saved))]))

; [contract] store-lookup: Store -> RMFAE-Value
; [purpose] to get the value that corresponds to the address of store
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "No value at address")]
    [aSto (location value rest-store)
          (if (= location address)
              value
              (store-lookup address rest-store))]))

; [contract] malloc: Store -> integer
; [purpose] to assign the address sequentially
(define (malloc st)
  (+ 1 (max-address st)))

(define (max-address st)
  (type-case Store st
    [mtSto() 0]
    [aSto (n v st) (max n (max-address st))]))

; Solved by myself: Y
; Time taken: 1 hours
; [contract] interp: BMFAE DefrdSub Store -> Value*Store
; [purpose] to change BMFAE DefredSub and Store into Value*Store data type
(define (interp bmfae ds st)
  (type-case BMFAE bmfae
    [num (n) (v*s (numV n) st)]
    [add (l r) (type-case Value*Store (interp l ds st)
                 [v*s (l-value l-store)
                      (type-case Value*Store (interp r ds l-store)
                        [v*s (r-value r-store)
                             (v*s (num+ l-value r-value) r-store)])])]
    [sub (l r) (type-case Value*Store (interp l ds st)
                 [v*s (l-value l-store)
                      (type-case Value*Store (interp r ds l-store)
                        [v*s (r-value r-store)
                             (v*s (num- l-value r-value) r-store)])])]
    [mul (l r) (type-case Value*Store (interp l ds st)
                 [v*s (l-value l-store)
                      (type-case Value*Store (interp r ds l-store)
                        [v*s (r-value r-store)
                             (v*s (num* l-value r-value) r-store)])])]
    [id (name) (v*s (store-lookup (lookup name ds) st) st)]
    [fun (p b) (v*s (closureV p b ds) st)]
    [refun (p b) (v*s (refclosV p b ds) st)]
    [app (f a) (type-case Value*Store (interp f ds st)
                 [v*s (f-value f-store)
                      (type-case BMFAE-Value f-value
                        [closureV (c-param c-body c-ds)
                                  (type-case Value*Store (interp a ds f-store)
                                    [v*s (a-value a-store)
                                         (local ([define new-address (malloc a-store)])
                                           (interp c-body 
                                                   (aSub c-param new-address c-ds)
                                                   (aSto new-address a-value a-store)))])]
                        [refclosV (rc-param rc-body rc-ds)
                                  (local ([define address (lookup (id-name a) ds)])
                                    (interp rc-body
                                            (aSub rc-param address rc-ds) f-store))]
                        [else (error interp "trying to apply a number")] )])]
    [newbox (val)
            (type-case Value*Store (interp val ds st)
                    [v*s (vl st1)
                         (local [(define a (malloc st1))]
                           (v*s (boxV a) (aSto a vl st1)))])]
    [openbox (bx-expr)
             (type-case Value*Store (interp bx-expr ds st)
               [v*s (bx-val st1)
                    (v*s (store-lookup (boxV-address bx-val) st1) st1)])]
    [setbox (bx-expr val-expr)
            (type-case Value*Store (interp bx-expr ds st)
              [v*s (bx-val st2)
                   (type-case Value*Store (interp val-expr ds st2)
                     [v*s (val st3) (aSto (boxV-address bx-val)
                                          val
                                          st3)])])]
    [seqn (a b)
          (type-case Value*Store (interp a ds st)
                     [v*s (a-value a-store)
                          (interp b ds a-store)])]
    [setvar (id val-expr)
            (local [(define a (lookup id ds))]
              (type-case Value*Store (interp val-expr ds st)
                [v*s (val st)
                     (v*s val (aSto a val st))]))]))

; [contract] run: s-expr DefrdSub -> RCFAE-Value
; [purpose] to call parse and interp function in one call
(define (run sexp ds st)
  (interp (parse sexp) ds st))

(test (run '{with {a 3} {setvar a 5}} (mtSub) (mtSto))
      (v*s (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (mtSto)))))

(test (run '{with {a 3} {seqn {{fun {x} {setvar x 5}} a} a}} (mtSub) (mtSto))
      (v*s (numV 3) (aSto 2 (numV 5) (aSto 2 (numV 3) (aSto 1 (numV 3) (mtSto))))))

(test (run '{with {a 3} {seqn {{refun {x} {setvar x 5}} a} a}} (mtSub) (mtSto))
      (v*s (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (mtSto))))) 
