#lang racket

; Egecan Serbester
; 2019400231
; compiling: yes
; complete: yes

(provide (all-defined-out))

( define ( append lis1 lis2 )  ;appending a list
(cond ( ( null? lis1 ) lis2 )   ; if it is null than return lis2
(else (cons (car lis1 )         ; if it is not null than cons head of list1 and call the function recursively from rest
( append (cdr lis1 ) lis2 ) ) ) ) )

( define (member atm lis )   ;check member of list and check whether it's a list or not
(cond ( (or ( null? lis ) (not(list? lis) ) ) #f  )   ;if the list is null or that's not a list return false
( (eq? atm (car lis ) ) #t )         ;if it is equal to given input return true
(else (member atm (cdr lis ) ) ) ) )   ;else call function recursively from rest of the lis


(define (reverse lst)   ;reverse the list
  (if (null? lst) '()    ; if it is null return empty set
    (append (reverse (cdr lst)) (list (car lst))))) ;else append and call reverse function recursively

(define (hadi elem list list2 list3)
  #| list is a list that given,   list2 is the inner lists , list3 is the list contains all inner lists |#   
  (cond ( ( null? list ) (reverse(cons (reverse list2) list3 ) )  )
  #| if there is no element in given list then it returns the reverse of the union of reverse inner list and the list3 |#
        ( (eqv? elem (car list) ) (hadi elem (cdr list) '( ) (cons (reverse list2) list3 )  )  )
  #| if the head of list equal of wished element then it call that func recursively and with list1 as the rest of the given list
list2 as an empty list (because inner lists begin with no elem from the past), list3 as union of reverse of last inner list and last list3 |#
        ( else  (hadi elem (cdr list) (cons (car list) list2 ) list3 ) ) ) )
 #| else (list is not empty and head of list is not the wanted element) then call that function recursively with last of the list and appends head of list to list2
 because it appends the head as it proceeds I reverse the list2 in future steps |#

(define (hadi2 elem list list2 list3)   ;split without ()   only difference from split it is in the line below
  (cond ( (and ( null? list ) (null? list2) ) (reverse list3) )  ;if list2 is null don't print it out
        ( (null? list) (reverse(cons(reverse list2) list3) ) )  
        ( (eqv? elem (car list) ) (hadi2 elem (cdr list) '( ) (cons (reverse list2) list3 )  )  )
        ( else  (hadi2 elem (cdr list) (cons (car list) list2 ) list3 ) ) ) )

(define (split_null elem lst) (hadi2 elem lst '() '() ) )   ;the func from split without null elems



; 10 points
( define := (lambda args args)  )  ;return given values
; 10 points
(define (-- . args) (:= 'let args) )    ;return let and given args
; 10 points
(define (@ ParsedBinding Expr) (append ParsedBinding Expr) )   ;return append of parsedbinding and expression
; 20 points
(define (split_at_delim delim args) (hadi delim args '() '() ) )  #| the function that splitted the list from given elems |#
; 30 points
(define (parse_expr list1)
  (cond ( (null? list1)  )
        ( (member '+ list1) (cons '+ (map parse_expr (split_at_delim '+ list1) ) ) )   ;if it has + than split it and add head of the list + and call the function recursively for all splitted ones
        ( (member '* list1) (cons '* (map parse_expr (split_at_delim '* list1) ) ) )   ;if it has * than split it and add head of the list * and call the function recursively for all splitted ones
        ( (member '@ list1) (list 'let (bind (car list1) )  (parse_expr (caddr list1) ) ) ) ;if it has @ than make a list, add beginning of the list let and send bind left of the @, call expression function for right of the @
        (else (cond ( (not (list? (car list1) ) ) (car list1) )                 ;if there is one element print that
                    (else (parse_expr (car list1) ) ) ) ) ) )                          ;if it is a list sends it to the parse again
        
(define (bind list1)
  (cond ( (null? list1) )
        ;( (length list1) ) ) )
        ( (and (list? list1) (eqv? (length list1) 1) ) (bind (car list1) ) )   ; if it has unnecessary paranthesis it call the function again from head 
        ( (member '-- list1) (map assignm (split_null '-- list1) ) )        ; if it has -- than split it and call the function recursively for all splitted ones
        ;but split func different from others, it is not printed null list to prevent undesired things
        ( (member ':= list1) (assignm list1 ) ) ) )                         ;  if it has := it sends it to the assignment func
        
                                            

(define (assignm list1)  ;if it is assignment it comes there
   (cond ( (and (list? list1) (eqv? (length list1) 1) ) (assignm (car list1) ) ) ; if it has unnecessary paranthesis it call the function again from head
     ( (not(null? list1) ) (eval ( list ':= (car list1) (caddr list1) ) ) ) ) )  ; if it is not null then evaluate and return the := functions value
; 20 points
(define (eval_expr lst) (eval (parse_expr lst) ) )  ;return evaluated value of expression
