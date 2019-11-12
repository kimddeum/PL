HW#lang plai

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [mul (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun RCFAE?) (arg RCFAE?)]
  [if0 (test RCFAE?) (then RCFAE?) (else RCFAE?)]
  [rec (name symbol?) (fun-body RCFAE?) (fct-call RCFAE?)])

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RCFAE?) (ds DefrdSub?)]
  [exprV (expr RCFAE?) (ds DefrdSub?) (value (box/c (or/c false RCFAE-Value?)))])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value RCFAE-Value?) (ds DefrdSub?)]
  [aRecSub (name symbol?) (value-box (box/c RCFAE-Value?)) (ds DefrdSub?)])

; Solved by myself: Y
; Time taken: 5 minutes
; [contract] strict: RCFAE-Value -> RCFAE-Value
; [purpose] to reduce the expression to a value
(define (strict v)
  (type-case RCFAE-Value v
    [exprV (expr ds v-box)
           (if (not (unbox v-box))
              (local[(define v (strict (interp expr ds)))]
                (begin (set-box! v-box v)
                       v))
              (unbox v-box))]
    [else v]))

; Solved by myself: Y
; Time taken: 5 minutes
; [contract] num-op: (number number -> number) -> (RCFAE RCFAE -> RCFAE)
; [purpose] to calculate the numbers with corresponding operation
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n (strict x)) (numV-n (strict y))))))
(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))

; Solved by myself: No. I asked one of my colleagues to verify my logic for implementing recursion is right
; Time taken: 4 hours
; [contract] parse: s-expr -> RCFAE 
; [purpose] to convert sexp to RCFAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'rec (list n b) c) (rec n (parse b) (parse c))]
    [(list 'if0 tst then e) (if0 (parse tst) (parse then) (parse e))]
    [else (error 'parse "bad syntax: ~a" sexp)]))

; Solved by myself: Y
; Time taken: 5 minutes
; [contract] numzero?: RCFAE-Value -> boolean
; [purpose] to check wheter the value is zero or not
(define (numzero? n)
  (zero? (numV-n n)))

; Solved by myself: Y
; Time taken: 5 minutes
; [contract] lookup: symbol DefrdSub -> RCFAE-Value
; [purpose] to change symbol of DefrdSub into RCFAE-value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name value rest-ds)
          (if (symbol=? sub-name name)
              (strict value)
              (lookup name rest-ds))]
    [aRecSub (sub-name val-box rest-ds)
             (if (symbol=? sub-name name)
                 (unbox val-box)
                 (lookup name rest-ds))]))

; Solved by myself: Y
; Time taken: 5 minutes
; [contract] interp: RCFAE DefrdSub -> RCFAE-Value
; [purpose] to change RCFAE DefredSub into RCFAE-Value
(define (interp rcfae ds)
  (type-case RCFAE rcfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [mul (l r) (num* (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a)
         (local [(define ftn-v (strict(interp f ds)))
                 (define arg-v (exprV a ds (box #f)))]
           (interp (closureV-body ftn-v)
                   (aSub (closureV-param ftn-v)
                         arg-v
                         (closureV-ds ftn-v))))]
    [if0 (test then else)
         (if (numzero? (interp test ds))
         (interp then ds)
         (interp else ds))]
    [rec (name fun-body fct-call)
      (local [(define value-holder (box(numV 12)))
              (define new-ds (aRecSub name value-holder ds))]
      (begin
        (set-box! value-holder (interp fun-body new-ds))
        (interp fct-call new-ds)))]))

; Solved by myself: Y
; Time taken: 5 minutes
; [contract] run: s-expr DefrdSub -> RCFAE-Value
; [purpose] to call parse and interp function in one call
(define (run sexp ds)
  (interp (parse sexp) ds))

;(test (lookup 'x (aSub 'x (numV 1) (mtSub))) (numV 1))
;(test (run '{{fun {x} {+ 1 x}} 10} (mtSub)) (numV 11))
;(run '{if0 1 {+ 1 2} {+ 1 3}} (mtSub))
(test (run '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}} (mtSub)) (numV 8))
(test (run '{rec {fac {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}} {fac 10}} (mtSub)) (numV 3628800))