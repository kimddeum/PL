#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: about a minute
; [contract] dollar->won: number -> number
; [purpose] To convert dollar to won
; [tests] (test (dollar->won 1) 1200)
;         (test (dollar->won 2) 2400)
;         (test (dollar->won 5) 6000)

(define (dollar->won x) (* x 1200))

(test (dollar->won 1) 1200)
(test (dollar->won 2) 2400)
(test (dollar->won 5) 6000)

; Problem 2:
; Solved by myself: Y
; Time taken: about 20 minutes
; [contract] first_digit: number -> number
; [purpose] to get the first digit of three digit number
; [tests] (test (first_digit 100) 1)
;         (test (first_digit 587) 5)
;         (test (first_digit 964) 9)

(define (first_digit x) (/ (- x (modulo x 100)) 100))

(test (first_digit 100) 1)
(test (first_digit 587) 5)
(test (first_digit 964) 9)

; [contract] second_digit: number -> number
; [purpose] to get the second digit of three digit number
; [tests] (test (second_digit 100) 0)
;         (test (second_digit 587) 8)
;         (test (second_digit 964) 6)

(define (second_digit x) (/ (- (modulo x 100) (modulo x 10)) 10))

(test (second_digit 100) 0)
(test (second_digit 587) 8)
(test (second_digit 964) 6)

; [contract] thrid_digit: number -> number
; [purpose] to get the third digit of three digit number
; [tests] (test (third_digit 100) 0)
;         (test (third_digit 587) 7)
;         (test (third_digit 964) 4)

(define (third_digit x) (modulo x 10))

(test (third_digit 100) 0)
(test (third_digit 587) 7)
(test (third_digit 964) 4)

; [contract] digit_sum: number -> number
; [purpose] to sum all digits of three integer number
; [tests] (test (digit_sum 100) 1)
;         (test (digit_sum 587) 20)
;         (test (digit_sum 964) 19)

(define (digit_sum x)
  (cond
    [(= x 0) 0]

    [else (+ (first_digit x) (second_digit x) (third_digit x))]))

(test (digit_sum 100) 1)
(test (digit_sum 587) 20)
(test (digit_sum 964) 19)

; Problem 3:
; Solved by myself: Y
; Time taken: about 5 minutes
; [contract] volume-sphere: number -> number
; [purpose] to calculate the volumne of sphere
; [tests] (test (volume-sphere 1) 4.18666666)
;         (test (volume-sphere 2) 33.4933333)

(define (volume-sphere x) (* (/ 4 3) 3.14 x x x))

(test (volume-sphere 1) 4.18666666)
(test (volume-sphere 2) 33.4933333)

; Problem 4:
; Solved by myself: Y
; Time taken: about 5 minutes
; [contract] is-even?: number -> boolean
; [purpose] to determine whether x is an even number or not
; [tests] (test (is-even? 0) true)
;         (test (is-even? 5) false)
;         (test (is-even? 100) true)

(define (is-even? x) (= (modulo x 2) 0))

(test (is-even? 0) true)
(test (is-even? 5) false)
(test (is-even? 100) true)

; Problem 5:
; Solved by myself: Y
; Time taken: about 5 minutes
; [contract] factorial: number -> number
; [purpose] to calculate the factorial of x
; [tests] (test (factorial 0) 1)
;         (test (factorial 2) 2)
;         (test (factorial 5) 120)

(define (factorial x)
  (cond
    [(= x 0) 1]
    [else (* x (factorial (- x 1)))]))

(test (factorial 0) 1)
(test (factorial 2) 2)
(test (factorial 5) 120)

; [contract] combination: number -> number
; [purpose] to calculate the combination of n and k
; [tests] (test (combination 3 2) 3)
;         (test (combination 10 3) 120)
;         (test (combination 10 5) 252)

(define (combination n k) (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(test (combination 3 2) 3)
(test (combination 10 3) 120)
(test (combination 10 5) 252)

; Problem 6:
; Solved by myself: Y
; Time taken: about an hour
; [purpose] to define the type PERSON which has Professor, UndergraduateStudent, and GraduateStudent
; [tests] (test (GraduateStudent? graduate1) true)
;         (test (Professor professor1) true)
;         (test (GraduateStudent? undergraduate1) false)

(define-type PERSON
  (Professor (courses number?)
             (projects number?))
  (UndergraduateStudent (courses number?))
  (GraduateStudent (courses number?)
                   (papers number?)))

(define graduate1(GraduateStudent 4 2))
(define professor1(Professor 3 6))
(define undergraduate1(UndergraduateStudent 6))

(test (GraduateStudent? graduate1) true)
(test (Professor? professor1) true)
(test (GraduateStudent? undergraduate1) false)

; [contract] have-courses: PERSON -> number
; [purpose] to get the number of courses either students taken or professor taught
; [tests] (test (have-courses graduate1) 4)
;         (test (have-courses professor1) 3)
;         (test (have-courses undergraduate1) 6)

(define (have-courses p)
  (type-case PERSON p
    [Professor (c prog) c]
    [UndergraduateStudent (c) c]
    [GraduateStudent (c ppr) c]))

(test (have-courses graduate1) 4)
(test (have-courses professor1) 3)
(test (have-courses undergraduate1) 6)

; [contract] ready-to-graduate: PERSON -> boolean
; [purpose] to check whether graudate student can graduate or not
; [tests] (test (ready-to-graduate graduate1) false)
;         (test (ready-to-graduate graduate2) true)
;         (test (ready-to-graduate graduate3) true)

(define (ready-to-graduate p)
  (type-case PERSON p
    [Professor (c proj) false]
    [UndergraduateStudent (c) false]
    [GraduateStudent (c ppr)
                     (cond
                       [(>= ppr 3) true]
                       [else false]
                       )]))

(define graduate2(GraduateStudent 3 3))
(define graduate3(GraduateStudent 2 5))

(test (ready-to-graduate graduate1) false)
(test (ready-to-graduate graduate2) true)
(test (ready-to-graduate graduate3) true)

; Problem 7:
; Solved by myself: N (JC helped me on how to get the character from the list)
; Time taken: about an hour
; [contract] name-alphabet: list -> list
; [purpose] to change the specific characters with spedicif strings
; [tests] (test (name-alphabet '(a b c)) '(alice unnamed cherry))
;         (test (name-alphabet '(a j k e)) '(alice jc kate unnamed))
;         (test (name-alphabet '(b d l o)) '(unnamed unnamed unnamed unnamed))

(define (name-alphabet lst)
  (cond
    [(empty? lst) (append empty)]
    [(symbol=? (first lst) 'a) (append (list 'alice) (name-alphabet(rest lst)))]
    [(symbol=? (first lst) 'c) (append (list 'cherry) (name-alphabet(rest lst)))]
    [(symbol=? (first lst) 'j) (append (list 'jc) (name-alphabet(rest lst)))]
    [(symbol=? (first lst) 'k) (append (list 'kate) (name-alphabet(rest lst)))]
    [else (append (list 'unnamed) (name-alphabet(rest lst)))]))

(test (name-alphabet '(a b c)) '(alice unnamed cherry))
(test (name-alphabet '(a j k e)) '(alice jc kate unnamed))
(test (name-alphabet '(b d l o)) '(unnamed unnamed unnamed unnamed))

; Problem 8:
; Solved by myself: Y
; Time taken: about an hour
; [contract] update-namme: symbol, symbol, list of symbols -> list of symbols
; [purpose] to update the old symbol with the new one
; [tests] (test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
;         (test (update-name 'difficult 'exciting (cons 'pl (cons 'is (cons 'difficult empty)))) '(pl is exciting))

(define (update-name old new lst)
  (cond
    [(empty? lst) (append empty)]
    [(symbol=? (first lst) old) (append (list new) (update-name old new (rest lst)))]
    [else (append (list (first lst)) (update-name old new (rest lst)))]))

(test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
(test (update-name 'difficult 'exciting (cons 'pl (cons 'is (cons 'difficult empty)))) '(pl is exciting))
