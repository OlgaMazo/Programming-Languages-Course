; The ROL BNF and Parsing code:
#lang pl

;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

#|
Q.1.1:
My difficulty with this question was to understand what exactly should be done and how to construct the grammar correctly,
I read the question several times, helped with the questions asked in the forum and finally figured out how to complete the code
(I helped to Daniel Zilpa to understand the BNF)

BNF for the ROL language:

<ROL> ::= { reg-len = <Num> <RegE> }

<RegE> ::= <Bits>
         | { and <RegE> <RegE> }
         | { or <RegE> <RegE> }
         | { shl <RegE> }

<Bits> ::= <BIT> | <BIT> <Bits>


3 examples for ROL words how they are derived from the BNF:

"{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}" -> <ROL> -> { reg-len = 2 <RegE> } ->
{ reg-len = 2 { or <RegE> <RegE> } } -> { reg-len = 2 { or { and <RegE> <RegE> } <Bits> } } ->
{ reg-len = 2 { or { and { shl <RegE> } <Bits> } 1 <Bits> } } -> { reg-len = 2 { or { and { shl <Bits> } 1 <Bits> } 1 0 } } ->
{ reg-len = 2 { or { and { shl 1 <Bits> } 1 0 } 1 0 } } -> { reg-len = 2 { or { and { shl 1 0 } 1 0 } 1 0 } }

"{ reg-len = 4 {or {shl {1 0 1 0}}{shl {1 0 1 0}}}}" -> <ROL> -> { reg-len = 4 <RegE> } ->
{ reg-len = 4 { or <RegE> <RegE> } } -> { reg-len = 4 { or { shl <RegE> } { shl <RegE> } } } ->
{ reg-len = 4 { or { shl <Bits> } { shl <Bits> } } } -> { reg-len = 4 { or { shl 1 <Bits> } { shl 1 <Bits> } } } ->
{ reg-len = 4 { or { shl 1 0 <Bits> } { shl 1 0 <Bits> } } } -> { reg-len = 4 { or { shl 1 0 1 <Bits> } { shl 1 0 1 <Bits> } } } ->
{ reg-len = 4 { or { shl 1 0 1 1 } { shl 1 0 1 1 } } }

"{ reg-len = 3 { and {and {shl {1 0 1}} {shl {1 0 1}}} {1 0 1}}}" -> <ROL> -> { reg-len = 3 <RegE> } ->
{ reg-len = 3 { and <RegE> <RegE> } } -> { reg-len = 3 { and { and <RegE> <RegE> } <Bits> } } ->
{ reg-len = 3 { and { and { shl <RegE> } { shl <RegE> } } 1 <Bits> } } -> { reg-len = 3 { and { and { shl <Bits> } { shl <Bits> } } 1 0 <Bits>} } ->
{ reg-len = 3 { and { and { shl 1 <Bits> } { shl 1 <Bits> } } 1 0 1} } -> { reg-len = 3 { and { and { shl 1 0 <Bits>} { shl 1 0 <Bits>} } 1 0 1} } ->
{ reg-len = 3 { and { and { shl 1 0 1} { shl 1 0 1} } 1 0 1} }
|#

;;Q.1.2:
;;RegE abstract syntax trees
(define-type RegE
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Shl RegE])

;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
#|
list->bit-list
This function receives some list and returns a list of bits (0 or 1)
|#
 (: list->bit-list : (Listof Any) -> Bit-List)
 ;; to cast a list of bits as a bit-list
 (define (list->bit-list lst)
  (cond [(null? lst) null]
  [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))] ;;if the first organ is 1 - create cons where the first 1 and the rest sent for recursive calculation in the same function 
  [else (cons 0 (list->bit-list (rest lst)))])) ;;if the first organ is not 1 - create cons where the first 0 and the rest sent for recursive calculation in the same function 

 (: parse-sexpr : Sexpr -> RegE)
 (define (parse-sexpr sexpr) ;; to convert the main s-expression into ROL
  (match sexpr
   [(list reg-len = (number: n) Sexpr) ;;checking whether the expression we got is indeed appropriate form - " reg-len = len Sexpr "
     (if (> n 0) ;;checking if len is at least 1
         (parse-sexpr-RegL Sexpr n) ;;if yes - send Sexpr and len to parser
         (error 'parse-sexpr "the list length must be at least 1 ~s" sexpr))] ;;if not - error
   [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) ;;if the expression is not appropriate form - error

 (: parse-sexpr-RegL : Sexpr Number -> RegE)
 (define (parse-sexpr-RegL sexpr reg-len)  ;; to convert s-expressions into RegEs
   (match sexpr
    [(list (and a (or 1 0)) ... )
     (if (= (length a) reg-len) ;;checking whether the length of the list matches the number len
         (Reg(list->bit-list a)) ;;if yes - make the list a list of bits
         (error 'parse-sexpr "wrong number of bits in ~s" a))] ;;if not - error
    [(list 'and first second) ;;if it expression of type - and _ _ - send it to And variant
     (And (parse-sexpr-RegL first reg-len) (parse-sexpr-RegL second reg-len))]
    [(list 'or first second) ;;if it expression of type - or _ _ - send it to Or variant
     (Or (parse-sexpr-RegL first reg-len) (parse-sexpr-RegL second reg-len))] 
    [(list 'shl first) ;;if it expression of type - sh _ _ - send it to Shl variant
     (Shl (parse-sexpr-RegL first reg-len))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))  

 (: parse : String -> RegE)
 (define (parse input)  ;; parses a string containing a RegE expression to a RegE AST
  (parse-sexpr (string->sexpr input)))

;;tests Q1:
(test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 2 {or {shl {1 0}} {shl {1 0}}}}") => (Or (Shl (Reg '(1 0))) (Shl (Reg '(1 0)))))
(test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{ reg-len = 2 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
(test (parse "{ reg-len = 5 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
(test (parse "{ reg-len = 0 {and {1 1 1} {0 1 1}}}") =error> "the list length must be at least 1")
(test (parse "{ reg-len 3 {or {1 1 1 1} {0 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 3 {{1 1 1 1} {0 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 3 {or {1 1 1 1}}}") =error> "bad syntax in")

#|
Q2:
In this question I had trouble understanding what the problem really was (in 2.1),I tried to cut the phrase several times myself and saw that 2 different
results can be reached - so I understood what the problem was and managed to move forward with the question.
I try to understand the question with Daniel Zilpa.

Q2.a + 2.b:
The problem with this expression is that there is no set order here, it is unknown what to do first.
This problem can create ambiguity (as we learned in class).
A solution to this problem can be defining the order of operations, for example we decide that we work from left to right.

BNF for the MAE language:

<MAE> ::= {seq <AE>}
        | {seq <SET>}

<AE> ::= <num> 
       | {+ <AE> <AE>} 
       | {- <AE> <AE>} 
       | {* <AE> <AE>} 
       | {/ <AE> <AE>}

<GET> ::= get 
       | <num> 
       | {+ <GET> <GET>} 
       | {- <GET> <GET>} 
       | {* <GET> <GET>} 
       | {/ <GET> <GET>} 

<SET> ::= {set <AE>} <GET> 
       |  {set <AE>} <HELPER> <GET> 

<HELPER> ::= {set <GET>} 
          |  {set <GET>} <HELPER> 


3 derivation process for 3 different MAE expressions:

314106766 => {seq <SET>} -> {seq {set <AE>} <GET>} -> {seq {set {- <AE> <AE>}} <GET>} -> {seq {set {- <num> <num>}} <GET>} ->
{seq {set {- <num> <num>}} {* <GET> <GET>}} -> {seq {set {- <num> <num>}} {* get get}} -> {seq {set {- 314 66}} {* get get}}

314106766 => {seq <AE>} -> {seq {* <AE> <AE>} -> {seq {* <num> <num>} -> {seq {* 766 41}}

314106766 => {seq <SET>} -> {seq {set <AE>} <HELPER> <GET>} -> {seq {set {+ <AE> <AE>}} <HELPER> <GET>} -> {seq {set {+ <num> <num>}} {set <GET>} <GET>} ->
{seq {set {+ <num> <num>}} {set {- <GET> <GET>}} <GET>} -> {seq {set {+ <num> <num>}} {set {- <num> <num>}} <GET>} ->
{seq {set {+ <num> <num>}} {set {- <num> <num>}} {/ <GET> <GET>}} -> {seq {set {+ <num> <num>}} {set {- <num> <num>}} {/ get <num>}}
 {seq {set {+ 31 676}} {set {- 76 106}} {/ get 10}}
|#

#|
Q3:
The difficulty for me in this question was to understand how I use the foldl function and map.
When I understood how they both worked (by reading online) the question was much less complicated and it was clearer how to solve it.
(Daniel Zilpa helped me to think about the solution)

In this question we receives a list of numbers as input and return the sum of the number squares in the list as output

sum-of-squares
This function receives a list of numbers and returns the sum of the number squares.
The squares are calculated using an auxiliary function
|#
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares list)
  (foldl + 0 (map square-of-number list))) ;;first calculate all the squares (using map) and then sum all the squares (using foldl)

#|
square-of-number
This function receives a number as input and returns its square (multiplies the number by itself)
|#
(: square-of-number : Number -> Number)
(define (square-of-number num)
  (* num num))

;;tests Q3:
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(5 0)) => 25)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(5 -6 -7)) => 110)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(2.5 0 3.5)) => 18.5)

#|
Q4:
The difficulty with this question was to figure out the correct way to build the tree (more precisely what the Node variant gets,
to understand I looked at the tree-map function and tried to figure out how to build it recursively and after a few attempts I could understand)
I helped to Daniel Zilpa to think about the solution.

In this question we have definition to the BINTREE type, it have 2 variant: Node or Leaf.
The BINTREE have 3 functions:tree-map, tree-fold, and tree-reverse.
|#

;;Q4.a
;;define a new type BINTREE:
(define-type BINTREE
  ;;constructors:
  [Node BINTREE BINTREE]  
  [Leaf Number])

#|
tree-map
This function receives a numeric function f and a binary tree as input,
and returns tree with the same shape but using f(n) for values in its leaves
|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map func bintree) 
  (cases bintree
   [(Leaf value) (Leaf (func value))] ;;if it a leaf -activate the function func on it
   [(Node bintree1 bintree2) (Node (tree-map func bintree1) (tree-map func bintree2))])) ;;if it a node - send its two sons recursively to tree-map

;;tests Q4.b:
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 (Node (Leaf 3) (Node (Leaf 5)(Node (Leaf 7) (Leaf 2))))) => (Node (Leaf 4) (Node (Leaf 6)(Node (Leaf 8) (Leaf 3)))))
(test (tree-map sub1 (Node (Node (Leaf 10) (Leaf 12)) (Node (Leaf 5)(Node (Leaf 7) (Leaf 2))))) => (Node (Node (Leaf 9) (Leaf 11)) (Node (Leaf 4)(Node (Leaf 6) (Leaf 1)))))

#|
tree-fold
This function receives as input some function that accepts two values and returns one value (func2A),
another function (func1A) that receives a number and returns the same value as the previous function and a binary tree.
tree-fold function activates func1A function on all the leaves in the tree, then activates func2A on all the binary tree.
|#
(: tree-fold : All (A) (A A -> A) (Number -> A) BINTREE -> A)
(define (tree-fold func2A func1A bintree)
  (cases bintree
   [(Leaf value) (func1A value)] ;;if it a leaf - active func1A on it (func1A)
   [(Node bintree1 bintree2) (func2A (tree-fold func2A func1A bintree1) (tree-fold func2A func1A bintree2))])) ;;if it a node - send it son with the 2 functions (func2A func1A) recursively

;;tests Q4.d:
(test (tree-fold - add1 (Node (Leaf 3) (Node (Leaf 5)(Leaf 7)))) => 6)
(test (tree-fold * sub1 (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6)(Leaf 5)))) => 60)
(test (tree-fold + add1 (Node (Leaf 2) (Node (Leaf 5)(Leaf 9)))) => 19)
(test (tree-fold - sub1 (Node (Node (Leaf 8)(Leaf 5)) (Leaf 12) )) => -8)

;;flattens a binary tree to a list of its values in left-to-right order
(: tree-flatten : BINTREE -> (Listof Number))
(define (tree-flatten tree)
 (tree-fold (inst append Number) (inst list Number) tree))

;;tests Q4.e:
(test (tree-flatten (Node (Leaf 3) (Node (Leaf 5)(Leaf 7)))) => '(3 5 7))
(test (tree-flatten (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6)(Leaf 5)))) => '(2 4 6 5))
(test (tree-flatten (Node (Leaf 2) (Node (Leaf 5)(Leaf 9)))) => '(2 5 9))
(test (tree-flatten (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6)(Node (Leaf 7) (Leaf 8))))) => '(2 4 6 7 8))

#|
tree-reverse
This function receives as input binary tree and returns a tree that is its mirror image.
The function uses auxiliary function that helps it to switch between leaves (switch-nodes) and function tree-fold
|#
(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse bintree)
  (tree-fold switch-nodes Leaf bintree)) 

#|
switch-nodes
The function receives 2 binary trees and actives the Node constructor with the reverse order between the 2 trees
|#
(: switch-nodes : BINTREE BINTREE -> BINTREE)
(define (switch-nodes bintree1 bintree2)
  (Node bintree2 bintree1)) ;;active Node constructor with the reverse order between the 2 trees

;;tests Q4.g:
(test (equal? (reverse (tree-flatten (Node (Leaf 3) (Node (Leaf 5)(Leaf 7)))))
              (tree-flatten (tree-reverse (Node (Leaf 3) (Node (Leaf 5)(Leaf 7)))))) => true)
(test (equal? (reverse (tree-flatten (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6)(Node (Leaf 7) (Leaf 8))))))
              (tree-flatten (tree-reverse (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6)(Node (Leaf 7) (Leaf 8))))))) => true)
(test (equal? (reverse (tree-flatten (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6)(Leaf 5)))))
              (tree-flatten (tree-reverse (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6)(Leaf 5)))))) => true)
(test (equal? (reverse (tree-flatten (Node (Node (Leaf 2) (Leaf 4)) (Node (Leaf 6)(Leaf 5)))))
              (tree-flatten (tree-reverse (Node (Node (Leaf 3) (Leaf 5)) (Node (Leaf 6)(Leaf 5)))))) => false)

