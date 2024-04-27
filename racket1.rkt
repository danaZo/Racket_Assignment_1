#lang pl

#| Question 1

- The function: create-fixed-length-lists

- What the function does (according to the Question): consumes a list of numbers, and an Integer ‘n’,
and returns a list of lists, where each sublist contains n elements from the original list,  in the same order they appear in the original list.
note : If the total number of elements in the original list is not a multiple of n,
the final sublist may contain fewer than n elements.
In our words: the function receives list with numbers and a number, and needs to return the split of itself to n pieces in a form of sublists.

- Solving process: we have found the functions 'build-list' and 'list-ref' here: https://docs.racket-lang.org/plait/Predefined_Functions_and_Constants.html#%28part._.Lists%29

- Difficulities: how to write a function that receives more than one parameter, solution:https://docs.racket-lang.org/plait/Predefined_Functions_and_Constants.html
The error: Type Checker: parse error in type;
 bad syntax in type application: expected a type constructor
  given a type: Real
  in: (Number)
this is because in racket the type number is not directly used in type annotations. Solution: use integer
The result of the first test at the first run was '((1 2 3) (2 3 4) (3 4 5)) instead of 
'((1 2 3) (4 5 6) (7 8 9)), this is because at the list-ref part we used like this: (list-ref lst (+ i j))
Solution: list-ref lst (+ (* i num) j)
- Time to solve: 18:48 - 20:52

- General explanation of the solution: returning error if the list is empty or the number is 0 or less,
first usage of 'build-list' is to create list of lists,
second use is to create each sublist by extracting elemnts from the original list with 'list-ref'

|#

(: create-fixed-length-lists : (Listof Number)Integer -> (Listof (Listof Number)))
   (define (create-fixed-length-lists lst num)
     ;; check if the given list is not a pair (a non-empty list) or if the integer is less than or equal to 0
     (if (or (not (pair? lst)) (<= num 0))
        (error 'create-fixed-length-lists "Cant divide the list")
         ;; otherwise, use the 'build-list' function to create a list of sublists
          (build-list (ceiling (/ (length lst) num)) ;; number of sublists needed
                      ;;lambda func that takes param i (which is integer), represents the index of the sublist
                  (lambda ([i : Integer])
                    ;; create each sublist with the length of 'num' value
                    (build-list num
                                ;;lambda func that takes param j (which is integer), represents the index of the element in the sublist
                                (lambda ([j : Integer])
                                  (list-ref lst (+ (* i num) j)))))))) ;; extract element from original list based on indices


;; given tests
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 3) => '((1 2
3) (4 5 6) (7 8 9)))
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 9) => '((1 2 3
4 5 6 7 8 9)))

;;our test - covering the empty list condition
(test (create-fixed-length-lists '() 3) =error> "Cant divide the list")
(test (create-fixed-length-lists '(1 2 3) 0) =error> "Cant divide the list")
(test (create-fixed-length-lists '() 0) =error> "Cant divide the list")




#| Question 2a

- The function: nested-list-depth

- What the function does (according to the Question): calculates the maximum depth of
nesting in a list that may contain any type. The function should use recursion to traverse
the list and determine the depth of the deepest nested list.
In our words: the function receives list of lists and with recursion counting deepness of nested lists and returns integer represnting the max depth

- Solving process:

- Difficulities: type mismatch
  expected: (Listof (Listof Number))
  given: (List
          One
          (List Number Number)
          (List (List Positive-Byte))
          (List Number (List Positive-Byte)))
  in: (quote (1 (2 3) ((4)) (5 (6))))
Solution: (: nested-list-depth : Any -> Number)

- Time to solve: 20:52 - 21:45, the day after: 20:30-20:52

- General explanation of the solution: using a helper recursive function that returns a list of depths for each element in the input list.
the stop cond is when input list empty, the step is calculate depth of every element in the list.
generaly, the solution uses recursion to traverse the nested lists and calculates the depth at each level.
|#

;; helper func
(: max-depths : Any -> (Listof Number))
(define (max-depths lst)
  (if(not (list? lst))
          (error 'max-depths "The argumant is not a list")
  (cond
    ((null? lst) '()) ;; if the input list is empty, return an empty list
    (else
     (cons (nested-list-depth (match lst [(cons first rest) first])) ;; calc depth for the first element
           (max-depths (match lst [(cons first rest) rest]))))))) ;; going through the rest of the list recursivly

;;main
(: nested-list-depth : Any -> Number)
(define (nested-list-depth x)
  (cond
    ((not (pair? x)) 0)  ;; if not a pair, then depth is 0
    (else
     (add1 ;; increase by 1 for the current level
      (apply max ;; calc the max depth between all sublists
             (max-depths x)))))) ;; call helper func



;; given tests
(test (nested-list-depth '(1 (2 3) ((4)) (5 (6)))) => 3)
(test (nested-list-depth '(1 2 3)) => 1)
(test (nested-list-depth '()) => 0)
;;our tests
(test (nested-list-depth '((1) (2) (3) (4) (5))) => 2)
(test (nested-list-depth '(1 (2 (3 (4 (5)))))) => 5)
(test (nested-list-depth '( ( ( ( ()))))) => 4)
(test (max-depths 1) =error> "The argumant is not a list")

#| Question 2b

- The function: min&max-lists

- What the function does (according to the Question): consumes a list of lists (where the
type of the elements in the inner list may be any type)
The function returns a list
of lists – such that for each inner list lst (in the original list) the following is done:
1.If lst contains at least one number, then lst is replaced with a list of size
two, containing the minimum and maximum in lst
2.Otherwise, lst is
replaced with a null.

In our words: we recive list of lists, with any type, and for each inner list lst, if lst has at least one number,
then lst replaced with list contains the min and max numbers in lst, otherwise it replaced with null 

- Solving process: used 'ormap' func from the docs - https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._ormap%29%29
used 'filter' - https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29
used 'let*' - https://docs.racket-lang.org/reference/let.html
used 'apply','min','max', 'map' from the docs as well
- Difficulities: dealing with mixed types of elemntes , solution - 'filter'
using many new functions (apply, map, ormap, let*,filter) it took time to understand which function can help us and how to use it.

- Time to solve: 21:45 - 22:35

- General explanation of the solution: one helper function is checking whether there at least one number in a given list using ormap,
other helper function filters out the min and max numbers from the sub list

|#

;; helper function to check if a list contains at least one number
(: contains-number? : (Listof Any) -> Boolean)
(define (contains-number? lst)
  ;; 'number?' - checks if the arg is a number, 'ormap' checks if the predicate satisfied at the list with at least one element
  (ormap number? lst))

;; helper function to deal with a sub list based on the presence of numbers
(: check-sub-list : (Listof Any) -> (Listof Number))
(define (check-sub-list inner-list)
  (if (contains-number? inner-list) ;;calling the helper function of checking if there are numbers in the list
      ;; if inner list contains numbers, create a list with min and max numbers
      (let* ([numbers (filter number? inner-list)] ;;'filter' creates a new list with selected filter - a number
             [min-number (apply min numbers)] ;;find the minimum number in the filtered list
             [max-number (apply max numbers)]);;same but max number
        (list min-number max-number));;creating the list containing the min and max numbers
      '()));; if inner list doesn't contain numbers, replace it with a null

;;main func
(: min&max-lists : (Listof (Listof Any)) -> (Listof (Listof Number)))
(define (min&max-lists lists)
  (map check-sub-list lists)) ;; map the check-sub-list function to each sub list


;; given tests
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
;;our test
(test (min&max-lists '()) => '()) ;;empty list
(test (min&max-lists '((any "Benny" OP 8))) => '((8 8)));;only one kind of number




#| Question 3.1 & 3.2

- The functions: Constructor for TaggedQueue, Enqueue Operation

- What the functions does (according to the Question):
 1. data structure called `TaggedQueue`
 Each element in this queue is tagged with a unique identifier (ID)
 Empty Queue (`EmptyTQ`): a constructor for an empty `TaggedQueue`.
 2. This function should accept an ID
(symbol), a value (any type), and an existing `TaggedQueue`, returning a new
`TaggedQueue` with the element added. 

- Difficulities: Type Checker: Declaration for `EmptyTQ' provided, but `EmptyTQ' is defined in another module
  in: EmptyTQ
  sol: we did a function define for creating an empty queue but it wasn't needed

- Time to solve: 19:39 - 20:49 (because the difficulity.. :') )

- General explanation of the solution: each element in the TaggedQueue is tagged with a Symbol (ID) and any value.
EmptyTQ generates an empty TaggedQueue (no params), Enqueue adds new tagged element to the TaggedQueue.
|#


;; defining the data structure
(define-type TaggedQueue
  [EmptyTQ]
  [Enqueue Symbol Any TaggedQueue])


(test (EmptyTQ) => (EmptyTQ))
(test (Enqueue 'x 42 (EmptyTQ)) => (Enqueue 'x 42 (EmptyTQ)))


#| Question 3.3

- The function: search-queue

- What the function does (according to the Question): This function should take an ID and a
`TaggedQueue` as inputs, returning the value associated with the first occurrence of the
ID in the queue, or `#f` if not found.
 
- Difficulities: understand how to go over the queue recursivly

- Time to solve:20:56 - 21:24

- General explanation of the solution: receiveing Symbol (which is the ID) and a TaggedQueue, returning Any ( because the element type is Any)
 or false if not found.
 using 'cases' (we learned it at third lecture) we check which type the queue is - id it's empty then return false,
 otherwise, recursively go through the queue, check if every element is the one we serch for by checking if the id that we search,
 equals to the symbol the element has. if does - returns the elemnt , otherwise calling the function again.
|#
(: search-queue : Symbol TaggedQueue -> (U Any #f))
(define (search-queue id tqueue)
  (cases tqueue
    [(EmptyTQ) #f]
    [(Enqueue symbol any tq)
     (if (eq? id symbol)
         any
         (search-queue id tq))]))

;; given test
(test (search-queue 'x (Enqueue 'x 42 (EmptyTQ))) => 42)
;; our tests
(test (search-queue 'x (EmptyTQ)) => #f) ;;empty queue can't search
(test (search-queue 'y (Enqueue 'x 42 (EmptyTQ))) => #f);;id not exists
(test (search-queue 'x (Enqueue 'x 42 (EmptyTQ))) => 42);;id is first elemnt
(test (search-queue 'y (Enqueue 'x 42 (Enqueue 'y "Hello" (EmptyTQ)))) => "Hello");;id not first element
(test (search-queue 'z (Enqueue 'x 42 (Enqueue 'y "Hello" (EmptyTQ)))) => #f) ;;id not exists

#| Question 3.4

- The function: dequeue-queue

- What the function does (according to the Question): This function should remove the
first element from the `TaggedQueue` and return the modified queue. If the queue is
empty, it should return `#f`.
 
- Difficulities: 

- Time to solve: 21:25 - 21:44

- General explanation of the solution: the function receives a TaggedQueue and return TaggedQueue without
the first element. if it was already empty then return false.
|#

(: dequeue-queue : TaggedQueue -> (U TaggedQueue #f))
(define (dequeue-queue tqueue)
  (cases tqueue
    [(EmptyTQ) #f]
    [(Enqueue id any tq) tq]
  ))

;; given test
(test (dequeue-queue (EmptyTQ)) => #f)
(test (dequeue-queue (Enqueue 'x 42 (EmptyTQ))) => (EmptyTQ))
;; our tests
(test (dequeue-queue (Enqueue 'x 42 (Enqueue 'y "hello" (EmptyTQ)))) => (Enqueue 'y "hello" (EmptyTQ))) ;;more than one item in quque
 ;; several instances types in ququ
(test (dequeue-queue (Enqueue 'x '(100 82 93 56) (Enqueue 'y '(a b c) (EmptyTQ))))
      => (Enqueue 'y '(a b c) (EmptyTQ)))
(test (dequeue-queue (Enqueue 'x 42 (Enqueue 'y "hello" (Enqueue 'z '(1 2 3) (EmptyTQ)))))
      => (Enqueue 'y "hello" (Enqueue 'z '(1 2 3) (EmptyTQ))))



#| Question 4

- The function: 

- What the function does (according to the Question): Design a BNF grammar for a
small programming language that supports string concatenation and variable assignments.
Your BNF should include the following components:
1. Variable Assignment: Variables can be assigned string values.
2. String Concatenation: Strings can be concatenated using a specific operator.
3. Variables and Strings: Define what constitutes a valid variable name and string format.
 
- Difficulities: assigning different types to each other
We thought we need to implement the language like in lecture 4, then we saw a mail where it was said
We don't need to implement - write only in comments the BNF
So the fist approach is left as a comment..
  

- Time to solve: 19:46 - 22:51

- General explanation of the solution: 
|#


#| The BNF grammar for a small programming language
  VarStr representing the var to string application that this grammar does
  Assignment is the form "x = "hello"" - Assigns the string "hello" to variable x
  Concatenation is the form  x ++ y - Concatenates the strings in variables x and y
  covering the cases when variable is a letter or more than one letter
  letter is the abc letters and uppercase also
  string can be empty, only one character, or several characters
  How to write "empty string" in BNF - https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
  Char can be any valid character there is expect \ and " that represents string and may result an error
  and ' that represent list and may result an error



<VarStr> := <AssignOrConc>
         | <AssignOrConc> <VarStr>

<AssignOrConc> := <Assignment>
               | <Concatenation>

<Assignment> := <Var> "=" <String>

<Concatenation> := <Variable> "=" <Variable> "++" <Variable>

<Variable> := <Letter>
           |<Letter> <Variable>

<String> := "" 
         |<Char>    
         |<Char><String> 
          
<Letter> := "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"

<Char> := <Character (excluding \ and " and ' which may cause problems)>
|#



#| fisrt approach
(define-type VarStr
  [Var Symbol] ;;  Define what constitutes a valid variable name 
  [Str String] ;; string format
  [Assignment VarStr VarStr] ;; Variables can be assigned string values
  )

(: parse-sexpr : Sexpr -> VarStr)
(define (parse-sexpr sxpr)
 (cond
   [(symbol? sxpr) (Var sxpr)]
   
   [(and (list? sxpr)
         (= (length sxpr) 3)
         (eq? '= (second sxpr)))
    (Assignment (parse-sexpr (first sxpr))
                (parse-sexpr (third sxpr)))]

   [else (error 'parse-sexpr "bad syntax in ~s" sxpr) ]
   )
  )

(test (parse-sexpr '(x = "hello")) => (Assignment (Var 'x) (Str "hello")))
(test (parse-sexpr 'x) => (Var 'x))
|#