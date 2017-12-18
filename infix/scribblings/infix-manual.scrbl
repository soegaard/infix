#lang scribble/manual
@(require scribble/eval
          scribble/basic
          scribble/bnf
          ; (planet cce/scheme:4:1/planet)
          (for-label ; (planet dherman/pprint:4)
           racket
           infix)
          "util.rkt")

@title[#:tag "top"]{Infix Expressions for Racket}
@author[(author+email "Jens Axel Søgaard" "jensaxel@soegaard.net")]

@defmodule[infix]{
This package provides infix notation for writing mathematical expressions.
}

@section{Getting Started}

A simple example, calculating 1+2*3.

@codeblock[#:indent 2]|{
#lang racket
(require infix)
($ "1+2*3")
}|

Or with @"@"-expressions:
@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
@${1+2*3}
}|

@defform[($ str ...)]{
A macro that processes infix syntax.
}

@subsection{Arithmetical Operations}

The arithmetical operations +, -, *, / and ^ is written with standard
mathematical notation. Normal parentheseses are used for grouping.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
@${2*(1+3^4)}   ; evaluates to 164
}|

@subsection{Identifiers}
Identifiers refer to the current lexical scope:

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
(define x 41)
@${x+1}   ; evaluates to 42
}|

@subsection{Application}

Function application use square brackets (as does Mathematica).
Here @scheme[sqrt] is bound to the square root function defined
in the language after at-exp, here the racket language.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
(display (format "The square root of 64 is ~a\n" @${sqrt[64]}))
@${list[1,2,3]} ; evaluates to the list '(1 2 3)
}|

@subsection{Lists}

Lists are written with curly brackets {}.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
@${{1,2,1+2}}  ; evaluates to '(1 2 3)
}|


@subsection{List Reference}

List reference is written with double square brackets.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
(define xs '(a b c))
@${xs[[1]]}  ; evaluates to b
}|

@subsection{Anonymous Functions}

The syntax (λ ids . expr) where ids are a space separated list
of identifiers evaluates to function in which the ids are bound in
body expressions.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
  
@${ (λ.1)[] }          ; evaluates to 1 
@${ (λx.x+1)[2]}       ; evaluates to 3
@${ (λx y.x+y+1)[1,2]} ; evaluates to 4
}|

@subsection{Square Roots}

Square roots can be written with a literal square root:

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
@${√4}     ; evaluates to 2
@${√(2+2)}  ; evaluates to 2
}|

@subsection{Comparisons}
The comparison operators <, =, >, <=, and >= are available.
The syntaxes ≤ and ≥ for <= and >= respectively, works too.
Inequality is tested with <>.

@subsection{Logical Negation}
Logical negations is written as ¬.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
(define true #t)
@${¬true}      ; evaluates to #f
@${¬(1<2)}     ; evaluates to #f
}|

@subsection{Assignment}
Assignment is written with := .

@subsection{Sequencing}
A series of expresions can be evaluated by interspersing semi colons
between the expressions.

@verbatim[#:indent 2]|{
#lang at-exp racket
(require infix)
(define x 0)
@${ x:=1 ;  x+3 }  ; evaluates to 4
}|

@section{Examples}

@subsection{Example: Fibonacci}

This problem is from the Euler Project.

Each new term in the Fibonacci sequence is generated by adding the 
previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

Find the sum of all the even-valued terms in the sequence which do not 
exceed four million.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix "while.rkt")

(define-values (f g t) (values 1 2 0))
(define sum f)
@${ 
while[ g< 4000000,
  when[ even?[g], sum:=sum+g];
  t := f + g;
  f := g;
  g := t];
sum}}|

Here "while.rkt" is a file with this contents:
@codeblock[#:indent 2]|{
#lang racket
(provide while)
; SYNTAX (while expr body ...)
;   1. evaluate expr
;   2. if expr was true then evaluate body ... and go to 1.
;   3. return (void)
(require (for-syntax syntax/parse))
(define-syntax (while stx)
  (syntax-parse stx
    [(_while expr body ...)
     #'(let loop () (when expr body ... (loop)))]))}|

@subsection{Example: Difference Between a Sum of Squares and the Square of a Sum}

This problem is from the Euler Project.

The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,
   (1 + 2 + ... + 10)^2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural 
numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred 
natural numbers and the square of the sum.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix "while.rkt")

(define n 0)
(define ns 0)
(define squares 0)
(define sum 0)
@${
sum:=0;
while[ n<100,
  n := n+1;
  ns := ns+n;
  squares := squares + n^2];
ns^2-squares
}
}|

The example above also shows that Scribble syntax can be used.

@subsection{Example: Pythagorean Triplets}

This example is from the Euler Project.

A Pythagorean triplet is a set of three natural numbers, a,b,c for which,
   a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)

(let-values ([(a b c) (values 0 0 0)])
  (let/cc return
    (for ([k (in-range 1 100)])
      (for ([m (in-range 2 1000)])
        (for ([n (in-range 1 m)])
          @${ a := k* 2*m*n;
              b := k* (m^2 - n^2);
              c := k* (m^2 + n^2);
              when[ a+b+c = 1000, 
                 display[{{k,m,n}, {a,b,c}}]; 
                 newline[];
                 return[a*b*c] ]})))))
}|

@subsection{Example: Miller Rabin Primality Test}

This example was inspired by Programming Praxis:

http://programmingpraxis.com/2009/05/01/primality-checking/


@codeblock[#:indent 2]|{
#lang at-exp racket
(require infix)
(require (only-in math/base random-integer))

(define (factor2 n)
  ; return r and s, s.t n = 2^r * s where s odd
  ; invariant: n = 2^r * s
  (let loop ([r 0] [s n])
    (let-values ([(q r) (quotient/remainder s 2)])
      (if (zero? r)
          (loop (+ r 1) q)
          (values r s)))))

(define (miller-rabin n)
  ; Input: n odd
  (define (mod x) (modulo x n))
  (define (expt x m)
    (cond [(zero? m) 1]
          [(even? m) @${mod[sqr[x^(m/2)] ]}]
          [(odd? m)  @${mod[x*x^(m-1)]}]))
  (define (check? a)
    (let-values ([(r s) (factor2 (sub1 n))])
      ; is a^s congruent to 1 or -1 modulo n ?
      (and @${member[a^s,{1,mod[-1]}]} #t)))
  (andmap check? 
          (build-list 50 (λ (_) (+ 2 (random-integer 0 (- n 3)))))))

(define (prime? n)
  (cond [(< n 2) #f]
        [(= n 2) #t]
        [(even? n) #f]
        [else (miller-rabin n)]))

(prime? @${2^89-1})
}|
