#lang pl

#|Question 1)
Time taken to solve: 20 min where most of that time was to understand how foldl and map work.
This function takes a list of numbers as an argument and return a number of the sum of squares of each number in the list.
We have helper function square that takes a number and returns the square of it,

|#

;; Computes the square of a given number.
(: square : Number -> Number)
(define (square x)
  (* x x))

;; Takes a list of numbers and produces the sum of the squares of all the numbers in the list.
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))



;;Test given:
(test (sum-of-squares '(1 2 3)) => 14)

;;Our tests:
(test (sum-of-squares '(2)) => 4)
(test (sum-of-squares '(-1)) => 1)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(1 2 3 4 5)) => 55)
(test (sum-of-squares '(2 3 5 7 11 13)) => 377)
(test (sum-of-squares '(-1 -2 -3 -4 -5)) => 55)

#|Question 2.1)
Time taken to solve: 2 hours.
What was difficult to us was mainly how to retun a function and to understand the params in the definition,
and wruite tail recursion
it took us a lot of time and we needed to watch again the lectures and tirgulim to succeed
|#

;;createPolynomial function takes a list of numbers and returns a function that accepts a number (Number) as input and produces another number (Number) as output.
;;calculatePoly is a recursive function that calculates the value of a polynomial given its coefficients (pl), a variable (x), a power (power), and an accumulator (stock).

(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial pl)
  (: calculatePoly : (Listof Number) Number Integer Number -> Number)
  (define (calculatePoly pl x power stock)
    (cond
      [(null? pl) stock]
      [else
       (calculatePoly (cdr pl) x
             (+ power 1)
             (+ stock (* (car pl) (expt x power))))]))

(define (polyX (x : Number))
    (calculatePoly pl x 0 0))
     polyX)



;;Test given:
(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>
(+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (p2345 4) =>
(+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5
(expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4
(expt 11 2)) (* 5 (expt 11 3))))

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6
(expt 11 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0) 
;;Our Test:
(define p0 (createPolynomial '(0)))
(test (p0 4) => 0)
(test (zero? (p0 123)))

(define p1 (createPolynomial '(1)))
(test (p1 4) => 1)

(define p2 (createPolynomial '(-1 2 -3)))
(test (p2 2) => -9)
(test (p2 -2) => -17)
(test (p2 0) => -1)

(define p3 (createPolynomial '(12 11 22)))
(test (p3 3) => (+ (* 12 (expt 3 0)) (* 11 (expt 3 1)) (* 22 (expt 3 2))))
(test (p3 3) => 243)



;;Question 2.2.1)

#| 
  The grammar: 
    <PLANG> ::= {{poly <AEs>} { <AEs> }}
    <AEs> ::= <AE> | <AE> <AEs>
    <AE>::=<num>
       | {+ <AE> <AE> }
       | {- <AE> <AE> }
       | {* <AE> <AE> }
       | {/ <AE> <AE> } 
|#


#|Question 2.2.2)

Time taken to solve: 8 hours approximatly and it was spread on few days.

it was difficult to understand how to write it according to what we have learned in the lecture
and adjust it to what needed in this assignment

|#

(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 

(define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE]) 

(: parse-sexpr : Sexpr -> AE) 
;; to convert s-expressions into AEs 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n)    (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))] 
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 


(: parse : String -> PLANG) 
;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str) 
    (let ([code (string->sexpr str)]) 
      (match code
        [(list (list 'poly) (list coeffs ...)) (error 'parse "at least one coefficient is required in ~s" code)]
        [(list (list 'poly coeffs ...) '()) (error 'parse "at least one point is required in ~s" code)]
        [(list (list 'poly coeffs ...) (list points ...)) (Poly (map parse-sexpr coeffs) (map parse-sexpr points))])))

(test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3)))) 
(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")


;;more
(test (parse "{{poly {+ 1 2} 2 3} {1 {* 1 1} 3}}") =>
      (Poly (list (Add (Num 1) (Num 2)) (Num 2) (Num 3))
            (list (Num 1) (Mul (Num 1) (Num 1)) (Num 3))))

(test (parse "{{poly {+ {* 3 2} 1}} {{- 5 2}}}") =>
      (Poly (list (Add (Mul (Num 3) (Num 2)) (Num 1))) (list (Sub (Num 5) (Num 2)))))



#|2.2.3)

we struggled to understand the eval-poly
what was needed in this part is to eval each element in the Poly expression,
and use the createPolynomial from first part of the question.

|#
 
             
(: eval : AE -> Number)    	 	
;; evaluates AE expressions to numbers
(define (eval expr) 
    (cases expr 
      [(Num n)  n] 
      [(Add l r) (+ (eval l) (eval r))] 
      [(Sub l r) (- (eval l) (eval r))] 
      [(Mul l r) (* (eval l) (eval r))] 
      [(Div l r) (/ (eval l) (eval r))]))  

(: eval-poly : PLANG -> (Listof Number)) 
(define (eval-poly p-expr) 
  (cases p-expr
    [(Poly r l) (map (createPolynomial (map eval r)) (map eval l))])) 
 
(: run : String -> (Listof Number)) 
;; evaluate a FLANG program contained in a string 
(define (run str) 
  (eval-poly (parse str)))

(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}") => '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}") => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4))  
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4))
;;more
(test (run "{{poly {! 1 2} 2 3 6 7 7} {1 2 3 7 9}}")  
=error> "bad syntax in (! 1 2)")
(test (run "{{poly 1} {1}}") => '(1))
(test (run "{{poly 0} {1 2 3 4 5 6 7}}") => '(0 0 0 0 0 0 0))
