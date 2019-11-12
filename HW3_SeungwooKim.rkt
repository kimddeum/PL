#lang plai

;data definition of WAE

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named_expr WAE?) (body WAE?)]
  [id (name symbol?)])

; [contract] re-order : list-of-sym -> list-of-sym
; [purpose] to remove duplicates and sort as ascending order
; [tests] (test (re-order '(a z e t t)) '(a e t z))
;         (test (re-order '(b b e d)) '(b d e))
;         (test (re-order '(a a a a b a)) '(a b))

(define (re-order list)
  (sort (remove-duplicates list) symbol<?))

;(test (re-order '(a z e t t)) '(a e t z))
;(test (re-order '(b b e d)) '(b d e))
;(test (re-order '(a a a a b a)) '(a b))

; [contract] all-ids : WAE -> list-of-sym
; [purpose] to get all the identifiers appeared on the WAE expression
; [tests] (test (all-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '(x))
;         (test (all-ids (with 'x (num 3) (add (id 'y) (num 3)))) '(y))
;         (test (all-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b x y))

(define (all-ids expr)
  (type-case WAE expr
    [num (n) empty]
    [add (l r) (re-order (append (all-ids l) (all-ids r)))]
    [sub (l r) (re-order (append (all-ids l) (all-ids r)))]
    [with (bound_id named_expr bound_body) (re-order (append (all-ids bound_body) (all-ids named_expr)) )]
    [id (s) (list s)]))

(test (all-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '(x))
(test (all-ids (with 'x (num 3) (add (id 'y) (num 3)))) '(y))
(test (all-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b x y))

; Task 1
; Solved by myself: Y
; Time taken: about 6 hours
; [contract] free-ids : WAE -> list-of-sym
; [purpose] to get the free identifiers from WAE expression
; [tests] (test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
;         (test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
;         (test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
;         (test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
;         (test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
;         (test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
;         (test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
;         (test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
;         (test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
;         (test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))


(define (free-ids expr)
  (type-case WAE expr
    [num (n) empty]
    [add (l r) (re-order (append (free-ids l) (free-ids r)))]
    [sub (l r) (re-order (append (free-ids l) (free-ids r)))]
    [with (bound_id named_expr bound_body) (re-order (append (remove* (list bound_id) (free-ids bound_body)) (free-ids named_expr)))]
    [id (s) (list s)]))

(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))

; Tast 2
; Solved by myself: Y
; Time taken: about 7 hours
; [contract] binding-ids : WAE -> list-of-sym
; [purpose] to get the binding identifiers from WAE expression
; [tests] (test (binding-ids (add (num 3) (sub (id 'x) (id 'y)))) '())
;         (test (binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) '(x y))
;         (test (binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) '(y))
;         (test (binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) '(x y))
;         (test (binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) '(w z))

(define (binding-ids expr)
  (type-case WAE expr
    [num (n) empty]
    [add (l r) (re-order (append (binding-ids l) (binding-ids r)))]
    [sub (l r) (re-order (append (binding-ids l) (binding-ids r)))]
    [with (bound_id named_expr bound_body) (re-order (append (list bound_id) (binding-ids named_expr) (binding-ids bound_body)))]
    [id (s) empty]))

;(test (binding-ids (add (num 3) (sub (id 'x) (id 'y)))) '())
;(test (binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) '(x y))
;(test (binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) '(y))
;(test (binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) '(x y))
;(test (binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) '(w z))

; Task 3
; Solved by myself: Y, but still cannot pass one testcase
; Time taken: about 10 hours
; [contract] bound-ids : WAE -> list-of-sym
; [purpose] to get the bound identifiers from WAE expression
; [tests] (test (bound-ids (with 'x (num 3) (add (id 'y) (num 3)))) '())
;         (test (bound-ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) '(x))
;         (test (bound-ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) '(x y))
;         (test (bound-ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) '(x y))
;         (test (bound-ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) '(x))
;         (test (bound-ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) '(x z))
;         (test (bound-ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) '(y))
;         (test (bound-ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) '(x y z))
;         (test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(a x))
;         (test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(x))

(define (bound-ids expr)
  (type-case WAE expr
    [num (n) empty]
    [add (l r) (re-order (append (bound-ids l) (bound-ids r)))]
    [sub (l r) (re-order (append (bound-ids l) (bound-ids r)))]
    [with (bound_id named_expr bound_body)
          (re-order (append (bound-ids named_expr) (append (remove* (remove* (list bound_id) (free-ids expr)) (all-ids bound_body)) (bound-ids bound_body))))]
    [id (s) empty]))

(test (bound-ids (with 'x (num 3) (add (id 'y) (num 3)))) '())
(test (bound-ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) '(x))
(test (bound-ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) '(x y))
(test (bound-ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) '(x y))
(test (bound-ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) '(x))
(test (bound-ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) '(x z))
(test (bound-ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) '(y))
(test (bound-ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) '(x y z))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(a x))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(x))
