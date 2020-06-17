#lang pl
#|
Q1
At first, I had a hard time figuring out how to go through the entire list and how to check the last two characters of each String.
I saw examples in presentations and on the internet (at Racket's website) and then understood.

The function plSuffixContained recieve a list of string and return the first string that it's suffix is "pl" - if there is such word, if not -
the function return false.
First I check if the list is empty , if it empty - return false
Then I check the first String in the list, I check 2 conditions:
1)if the length of the string at least 2
2)if the suffix of the string is "pl" (I check if the last letter is "l" and the penultimate letter is "p"
if the 2 conditions are true - I return the first String
if one of the conditions false - I send the rest of the list to the function plSuffixContained and do it all again.
|#
(: plSuffixContained : (Listof String) -> (U #f String))
(define (plSuffixContained list)
  (cond    
    [(null? list) #f]
    [(and (> (string-length (first list)) 1) ;;if string length>1 and the suffix is pl
          (and (equal? #\p (string-ref (first list) (-(string-length (first list))2)))
               (equal? #\l (string-ref (first list) (-(string-length(first list))1))))) (first list)]
    [else (plSuffixContained(rest list))])) ;;check the rest organs in the list

;;Tests Q1:
(test (plSuffixContained '("greg" "PLvsd" "rkfdsl" "gerfpl")) => "gerfpl")
(test (plSuffixContained '("nf" "fkds" "vfdpl" "ttpl" "arplpl")) => "vfdpl")
(test (plSuffixContained '("a" "t" "r" "t" "p" "l")) => false)
(test (plSuffixContained '()) => false)
(test (plSuffixContained '("ger" "rtir" "plg" "fkdl")) => false)


#|
Q2.1
On this question, my difficulty was to understand how I use the length of the list to represent the appropriate power
in the polynomial. I drew up lists of numbers on a page and the polynomial that should be accepted and after a few attempts I figured out how to do it

The function write-poly recieve a list of numbers (coefficients) and return polynomial.
The whole calculation is done by tail recursion - at each stage the current polynomial is calculated and the rest is added
I have used auxiliary functions for different cases (for example, if this is the first number - so as not to show the + sign before it).
To calculate the power number, I checked the length of the array at that moment and subtracted 1.
If the power is 0 - I did not show the power and only remain free organ.
If power is 1 - I only introduced x and not x^1.

helper-write-poly-1
This function gets the first number in the list and sends it to a suitable function for calculating the polynomial, so that it does not show + at first.
In this function, I handle a sequence of zeros and prevent a polynomial from starting with a + sign.
|#
(: helper-write-poly-1 : (Listof Number) String Number -> String)
(define (helper-write-poly-1 list str num)
 (cond
   
   [(and (= num 0)(= (length list)1)) str] ;;One number remains and it is 0 - will return the current string
   [(and (= num 0)(= (length list)2)) (calculate-poly (first(rest list)) 0 str)] ;;Two numbers remain and the first one is 0   
   [(= num 0) (helper-write-poly-1 (rest(rest list))(calculate-poly-first (first(rest list)) (-(length list)2) str) (first(rest(rest list))))] ;;The current number is 0 and there are more than 2 numbers on the list
   [else (helper-write-poly-2 (rest list) (calculate-poly-first (first list) (-(length list)1) str))])) ;;The number we check not 0 
  
#|
helper-write-poly-2
In this function I perform the calculation of the rest of the polynomial, after treating the case of the "+" in the beginning.
|#
(: helper-write-poly-2 : (Listof Number) String -> String)
(define (helper-write-poly-2 list str)
  (if (null? list)
      str
      (helper-write-poly-2 (rest list)(calculate-poly (first list) (-(length list)1) str))))

#|
calculate-poly-first
This function represents the first organ of the polynomial correctly (so the + does not appear at the beginning of the polynomial)
|#
(: calculate-poly-first : Number Number String -> String)
(define (calculate-poly-first num power str)
  (cond    
    [(= num 0) str] ;;coefficient=0    
    [(= power 0) (string-append str (number->string num))] ;;power=0 - don't show x^
    [(= power 1) (string-append (number->string num) "x")] ;;power=1 - show only num*x
    [else (string-append (number->string num)(string-append "x^" (number->string power)))])) ;;show num*x^power
  

#|
calculate-poly
This function represents the polynomial in the form of a string. We get a number that is the coefficient,
power (which is actually the length of the array less than 1)
and the current string - the polynomial we have so far and in this function another organ is added to it
converts the number to string and add to it x^power - and then add all this to the polynomial we have so far.
|#
(: calculate-poly : Number Number String -> String)
(define (calculate-poly num power str)
  (cond    
    [(= num 0) str] ;;coefficient=0  
    [(= power 0) (if(> num 0) (string-append str (string-append "+" (number->string num))) ;;power=0 - don't show x^
                              (string-append str (number->string num)))]
    [(= power 1) (if(> num 0) (string-append str (string-append "+" (string-append (number->string num) "x"))) ;;power=1 - show only num*x
                              (string-append str (string-append (number->string num) "x")))]
    [else (if(> num 0) (string-append str (string-append "+"(string-append (number->string num)(string-append "x^" (number->string power))))) ;;show num*x^power
                       (string-append str (string-append (number->string num)(string-append "x^" (number->string power)))))]))

#|
write-poly
The main function that gets a list of numbers and eventually returns a string representing a polynomial.
This function performs a private case test where the list is 2 in length and the first organ is 0 - and handles this case.
|#
(: write-poly : (Listof Number) -> String)
(define (write-poly list)
  (cond    
    [(null? list) ""] ;;empty list - return ""    
    [(and(=(length list)2) (=(first list)0)) (calculate-poly-first (first(rest list)) 0 "")] ;;list with 2 numbers where the first=0
    [else (helper-write-poly-1 list "" (first list))]))

;Tests Q2.1:
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(7 -8 -9 10)) => "7x^3-8x^2-9x+10")
(test (write-poly '(1)) => "1")
(test (write-poly '(0 0 0 0)) => "")
(test (write-poly '(2 0 0 0)) => "2x^3")
(test (write-poly '(-1)) => "-1")
(test (write-poly '(0)) => "")
(test (write-poly '(5 0)) => "5x")
(test (write-poly '(0 5)) => "5")
(test (write-poly '(0 -5)) => "-5")
(test (write-poly '(-5 0)) => "-5x")
(test (write-poly '(0 0 5)) => "5")
(test (write-poly '(0 0 -5)) => "-5")
(test (write-poly '(2 0 3)) => "2x^2+3")
(test (write-poly '(0 7 -4)) => "7x-4")
(test (write-poly '(7 0 3)) => "7x^2+3")
(test (write-poly '(7 0 -3)) => "7x^2-3")
(test (write-poly '(7 -1 3)) => "7x^2-1x+3")
(test (write-poly '(0 0 -2 3)) => "-2x+3")
(test (write-poly '(0 3 0 -4)) => "3x^2-4")


#|
Q2.2
The function compute-poly receives a list of numbers (representing a polynomial) and an x ,and returns the result of placing x in the polynomial.
Each time I sent the coefficient, the power, the x and the current result to an auxiliary function where I calculated
the current value (coefficient*x^power) and added it to the result.

helper-compute-poly
This function receives the entire list and x.
In each iteration, the function sends the first organ in the list currently to an auxiliary function - it's actually the coefficient,
the list length less than 1 - it's the strongest, x and the current result. Then run the function again with the rest of the list (without the first organ)
and continue until the function stops and returns the final result - it happens when the list is empty.
|#
(: helper-compute-poly : Number Number (Listof Number) -> Number)
(define (helper-compute-poly num_x result list)
  (if (null? list)
      result
      (helper-compute-poly num_x (calculate-result num_x (first list) (-(length list)1) result) (rest list))))

#|
calculate-result
In this function, occurs the calculation (coefficient*x^power) 
if power is equal to 0 - only the coefficient is added to the current result
if power is equal to 1 - do the calculation coefficient*x and add it to current result
in any other case - do the calculation coefficient*x^power and add it current res
|#
(: calculate-result : Number Number Integer Number -> Number)
(define (calculate-result num_x coefficient power result)
     (cond
       [(= power 0) (+ result coefficient)] ;;add only the coefficient to result
       [(= power 1) (+(* coefficient num_x) result)] ;;add coefficient*x to result
       [else (+(*(expt num_x power) coefficient) result)])) ;;add coefficient*x^power to result


#|
compute-poly
The main function that receives a list of numbers and x and eventually returns the result of placing x in the polynomial obtained from the number list
|#
(: compute-poly : Number (Listof Number) -> Number)
(define (compute-poly num_x list)
     (helper-compute-poly num_x 0 list))

;;Tests 2.2
(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly 0 '(5 -6 7)) => 7)
(test (compute-poly 2 '(3)) => 3)
(test (compute-poly -2 '(3 -4 0 -6)) => -46)
(test (compute-poly -3 '(0 5 0 -2)) => 43)


#|
Q3
The difficulty for me in this question was figuring out how to build data structure. I used a summary from last year that was an example
and then the current exercise was clearer to me.

Implementaion of data structure KeyStack. This data structure have 2 costructors (one empty that create EmptyKS and
the other adds organs to the structure and create Push).
KeyStack have two operation - search and pop
|#

(define-type KeyStack
  ;;Costructors:
  ;;Q3.1
  [EmptyKS]
  ;;Q3.2
  [Push Symbol String KeyStack])

#|
Q3.3
search-stack
The function recieves Symbol and KeyStack and returns the first String with the appropriate key, if no such string exists, the function returns false
First of all I check with which constructor the KeyStack was built and if it was built with Push I check if the key matches the key that
I am looking for - if so I return the value of the key (string), if not - sends the rest of the KeyStack back to the same function and do the same check.
In this section the difficulty for me was to understand how to work with the casese function, after reading it on the intranet it was simpler.
|#
(: search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack key stack)
  (cases stack ;;check if it is EmptyKS or Push KeyStack
    [(EmptyKS) #f]
    [(Push sym str sta) (if (equal? sym key) str (search-stack key sta))]
    ))


#|
Q3.4
pop-stack
The function receives a KeyStack and returns the same KeyStack without the first value.
If the received KeyStack is EmptyKS the function returns false
|#
(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack stack)
  (cases stack ;;check if it is EmptyKS or Push KeyStack
    [(EmptyKS) #f] ;;if stack empty return false
    [(Push sym str sta) sta])) ;;if it is Push stack return the stack without the first value

;;Tests 3: 
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 's (Push 'a "First" (Push 's "YES" (Push 'a "A" (EmptyKS))))) => "YES")
(test (search-stack 's (Push 'a "First" (Push 'c "YES" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (Push 'q "QR" (Push 'r "RS" (Push 's "ST" (Push 't "TU"(EmptyKS)))))) => (Push 'r "RS" (Push 's "ST" (Push 't "TU"(EmptyKS)))))
(test (pop-stack (EmptyKS)) => #f)

#|
Q4:

The difficulty with this functions was to understand what each function returns and when,
I wrote a few examples on the page and I understood.

is-odd? function receives a natural number as input and returns false if it is even (if it is not odd)
If the number it received = 0 - the function returns false, otherwise the function sends the number less 1 to the function is-even?
The functions continue to call oto each other and each time 1 subtracts from the original number until one of the functions returns true (is-even?)
or false (is-odd?)
|#
(: is-odd? : Natural -> Boolean)
(define (is-odd? x)
(if (zero? x)false
(is-even? (- x 1))))

#|
is-even? function receives a natural number as input and returns true if it is even
If the number it received = 0 - the function returns true, otherwise the function sends the number less 1 to the function is-odd?
|#
(: is-even? : Natural -> Boolean)
(define (is-even? x)
(if (zero? x)
true
(is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12))) 
(test (is-even? 12)) 
(test (not (is-odd? 0))) 
(test (is-even? 0)) 
(test (is-odd? 1)) 
(test (not (is-even? 1))) 


#|
The difficulty with this function was to understand what its input was (to understand that some of this input was a function in itself).
I went through the code and saw what the function was getting and then it was easier to understand.

every? function receives as input 2 things:
1)a function - this function receives as input somethig (A) and returns true or false (in our case the function is pred)
2)list of something, it must be the same type (A) as I mentioned in the previous section (in our case the list is lst)
the function every? returns true or false
every? returns true if lst is empty or if the following two conditions are met:
enabling the function pred on the rest of the list returns true
enabling the function every? with the function pred on the rest of the list returns true
|#
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
(define (every? pred lst) 
(or (null? lst) 
(and (pred (first lst)) 
(every? pred (rest lst))))) 


#|
all-even? function receives as input list of natural numbers and returns true if all the numbers in the list is even, otherwise returns false
The function uses the function every? we explained above (the function every? receives:
1)function that receives list of natural and return true/false
2)list of natural numbers)
every will return true only if all numbers in the list are even
|#
(: all-even? : (Listof Natural) -> Boolean)
(define (all-even? lst)
(every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))


#|
every2? function receives as input 4 things (2 functions and 2 lists):
1)a function - this function receives as input somethig (A) and returns true or false
2)a function - this function receives as input somethig (B) and returns true or false
3)list of something, it must be the same type(A) as I mentioned in section 1 
4)list of something, it must be the same type (B) as I mentioned in section 2 
every2? returns true if list1 is empty or if the following three conditions are met:
enabling the function pred1 on the first organ of list1 returns true
enabling the function pred2 on the first organ of list2 returns true
enabling the function every2? with the functions pred1 and pred2 on the rest of list1 and list2 (respectively) returns true
|#
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
(define (every2? pred1 pred2 lst1 lst2)
(or (null? lst1) ;;check is lst1 empty
(and (pred1 (first lst1)) ;;pred on first of lst1
(pred2 (first lst2)) ;;pred on first of lst2
(every2? pred1 pred2 (rest lst1) (rest lst2))))) 
