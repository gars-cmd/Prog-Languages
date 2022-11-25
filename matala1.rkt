#lang pl

;;;;;;;;;;;;;;;;;;;;;
;; Exercice 1.a
;;;;;;;;;;;;;;;;;;;;;

(: open-list : (Listof(Listof Number)) -> (Listof Number) )
(define (open-list lst)
        ;if the list is empty , return an empty list 
  (cond [(null? lst) '()]
        ;else return the concatanation of the first list and the recursive call to the rest of the list 
        [else (append (first lst) (open-list(rest lst)) )]
   )
)

;;;;;;;;;;;;;;;;;;;;;
;; Test of 1.a
;;;;;;;;;;;;;;;;;;;;;

(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
(test (open-list '() ) => '() )
(test (open-list '( (1 2 3) () (4 5 6 7) () () (10 11) )) => '(1 2 3 4 5 6 7 10 11))

;;;;;;;;;;;;;;;;;;;;;
;; Exercice 1.b
;;;;;;;;;;;;;;;;;;;;;

;help function to return the minimum of a list 
(: min-help : (Listof Number) -> Number)
(define (min-help lst)
        ;in case of empty list we return - infinity
  (cond [(null? lst) -inf.0]
        ;if the list is of size one then we return the only element 
        [(= 1 (length lst)) (+ 0.0 (first lst))]
        ;we return the minimum of the recursive call of the first element and the rest of the list
        [else (+ 0.0 (min (first lst) (min-help(rest lst))) ) ] 
       )
  )

;;;;;;;;;;;;;;;;;;;;;
;; Test of min-help
;;;;;;;;;;;;;;;;;;;;;

(test (min-help '(1 2 3 4)) => 1.0)
(test (min-help '(-1 2 3 4)) => -1.0)
(test (min-help '(-1)) => -1.0)
(test (min-help '(-4 -4 -4)) => -4.0)
(test (min-help '()) => -inf.0)


;help function to return the maximum of a list 
(: max-help : (Listof Number) -> Number)
(define (max-help lst)
        ;in case of empty list we return + infinity
  (cond [(null? lst) +inf.0]
        ;if the list is of size one then we return the only element 
        [(= 1 (length lst)) (+ 0.0 (first lst))]
        ;we return the minimum of the recursive call of the first element and the rest of the list
        [else (+ 0.0 (max (first lst) (max-help(rest lst))) ) ] 
       )
  )

;;;;;;;;;;;;;;;;;;;;;
;; Test of max-help
;;;;;;;;;;;;;;;;;;;;;

(test (max-help '(1 2 3 4)) => 4.0)
(test (max-help '(-4 2 3 4)) => 4.0)
(test (max-help '(-4 -4 -4)) => -4.0)
(test (max-help '(-1)) => -1.0)
(test (max-help '()) => +inf.0)

;The main min&max function
(: min&max : (Listof(Listof Number)) -> (Listof Number))
(define (min&max lst)
        ;In case of empty list the return the lists -inf ; +inf 
  (cond [(null? lst) '(-inf.0 +inf.0)]
        ; return a list of the minimum of the flattened list and the maximum of the flattened list 
        [else (list
                (min-help(open-list lst))
                (max-help(open-list lst))
               ) ]
  )
 )

#|
I had difficuties to handle the use of brackets ,
to handle the cast of int to floar and reverse . 
|#


;;;;;;;;;;;;;;;;;;;;;
;; Test of 1.b
;;;;;;;;;;;;;;;;;;;;;

(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((1) (3) () (5))) => '(1.0 5.0))
(test (min&max '(() () ())) => '(-inf.0 +inf.0))
(test (min&max '( (1 1) (-1 -1) (-4 5) (-5 -5) ())) => '(-5.0 5.0))
(test (min&max '( (3) () )) => '(3.0 3.0))




;;;;;;;;;;;;;;;;;;;;;
;; Exercice 1.c
;;;;;;;;;;;;;;;;;;;;;


(: min&max_apply : (Listof(Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
  ; if we didn't get anything return (-inf +inf)
  (cond [(null? lst) '(-inf.0 +inf.0)]
        ; if the content of the list of list is also empty return (-inf +inf)
        [(null? (open-list lst)) '(-inf.0 +inf.0)]
        ;else use the function apply to apply the min function to the list gotten after the open-list function
        [else (list (apply min(open-list lst))
                    (apply max(open-list lst)) )]
        )
  )


;;;;;;;;;;;;;;;;;;;;;
;; Test of 1.c
;;;;;;;;;;;;;;;;;;;;;

(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))
(test (min&max_apply '((1) (3) () (5))) => '(1 5))
(test (min&max_apply '(() () ())) => '(-inf.0 +inf.0))
(test (min&max_apply '( (1 1) (-1 -1) (-4 5) (-5 -5) ())) => '(-5 5))
(test (min&max_apply '( () (-1.5 2) (-3 6) (7 12.5) () )) => '(-3.0 12.5))



;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercice 2.1 & 2.2
;;;;;;;;;;;;;;;;;;;;;;;;

#|
 according to the given exemple from te assignment :
    we must return a table or empty table
      Then for the Add operation there is a need to be the symbol that represent a char
      and a string that w
|#

(define-type Table
  [EmptyTbl]
  [Add Symbol String Table]
 )

;;;;;;;;;;;;;;;;;;;;;
;; Test of 2.1 & 2.2
;;;;;;;;;;;;;;;;;;;;;

(test (EmptyTbl) => (EmptyTbl) )
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) =>(Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) =>(Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (Add 'c "" (EmptyTbl))))) =>(Add 'a "aa" (Add 'b "B" (Add 'a "A" (Add 'c "" (EmptyTbl))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercice 2.3
;;;;;;;;;;;;;;;;;;;;;;;;

; our result can be the string of the input or false 
(: search-table : Symbol Table -> (U String #f) )
(define (search-table input table)
  ;we check for the vals of the table 
  (cases table
        ;if we get an empty table then our input is not in
        [(EmptyTbl) #f]
        ; check for the next value of add
        [(Add key val table)
         ; if it's the same key we are looking for then we return the val
         (cond [(eq? key input) val]
               ;else we check for the rest of the element
               [else (search-table input table)]
          )
         ]
    )
  )


#|
Difficuties to know how to use the Add method and to understand that it can be used 
as kind of object that we link to represent the table 
|#


;;;;;;;;;;;;;;;;;;;;;
;; Test of 2.3
;;;;;;;;;;;;;;;;;;;;;

(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))))=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))))=> "AAA")
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'c ""(EmptyTbl)))))=> "")


;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercice 2.4
;;;;;;;;;;;;;;;;;;;;;;;;

(: remove-item : Table Symbol ->  Table )
(define (remove-item table input)
  (cases table
    ;if the table is an empty table the return also an empty table 
    [(EmptyTbl) (EmptyTbl)]
    ; if we get an Add component 
    [(Add key val table)
     ; we check if the content of the Add component is what we are looking for
     ; if it's the case we return the table without the component we found 
     (cond [(eq? key input) table]
           ;else we linked the element and call the recursion on the table 
           [else (Add key val (remove-item table input))]
      )
     ]
    )
  )

;;;;;;;;;;;;;;;;;;;;;
;; Test of 2.4
;;;;;;;;;;;;;;;;;;;;;

(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))) 'a)=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))) 'b)=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "" (Add 'b "B" (Add 'b "BB" (Add 'c "<>"(EmptyTbl))))) 'b)=> (Add 'a ""(Add 'b "BB" (Add 'c "<>"(EmptyTbl)))) )
