#lang racket
(provide string-lexeme->string)

;;; This file provides the function
;;;    string-lexeme->string : string -> string
;;; The function accepts a string containing an unparsed string literal
;;; and returns the corresponding string value.
;;; In other words, the following two expressions should produce equal values:
;;;    (string-lexeme->string s)  
;;;    (read (open-input-string (string-append "\"" s "\"")))


(define oct-digits (string->list "01234567"))
(define dec-digits (string->list "0123456789"))
(define hex-digits (string->list "0123456789abcdefABCDEF"))

(define (char-oct? c) (memv c oct-digits))
(define (char-dec? c) (memv c dec-digits))
(define (char-hex? c) (memv c hex-digits))

(define (utf-16-surrogate-pair->char hi lo)
  (integer->char
   (+ #x10000
      (arithmetic-shift (bitwise-and hi #x03ff) 10)
      (bitwise-and lo #x03ff))))

(define (string-lexeme->string l)
  (define n (string-length l))
  (define (substring4 i) (substring l i (+ i 4)))
  (define (substring3 i) (substring l i (+ i 3)))
  (define (substring2 i) (substring l i (+ i 2)))
  (define (substring1 i) (substring l i (+ i 1)))
  (define (parse-string i)
    (define (more  c) (cons c (parse-string (+ i 1))))
    (define (more2 c) (cons c (parse-string (+ i 2))))
    (cond
      [(= i n) '()]
      [else
       (define c (string-ref l i))
       (cond
         [(char=? c #\\)
          (cond
            [(= (+ i 1) n) '()] ; the string ended in a single \
            [else
             (define e (string-ref l (+ i 1)))
             (cond
               [(char=? e #\a)       (more2 #\u0007)]   ;  7 alarm
               [(char=? e #\b)       (more2 #\u0008)]   ;  8 backspace
               [(char=? e #\t)       (more2 #\tab)]     ;  9 tab           
               [(char=? e #\n)       (more2 #\newline)] ; 10 linefeed
               [(char=? e #\v)       (more2 #\vtab)]    ; 11 vertical tab
               [(char=? e #\f)       (more2 #\page)]    ; 12 formfeed
               [(char=? e #\r)       (more2 #\return)]  ; 13 return
               [(char=? e #\e)       (more2 #\u001B)]   ; 27 escape
               [(char=? e #\")       (more2 #\")]       ;    double-quote
               [(char=? e #\')       (more2 #\')]       ;    single-quote
               [(char=? e #\\)       (more2 #\\)]       ;    backslash
               [(char=? e #\x)       (parse-x-unicode (+ i 2))]
               [(char=? e #\u)       (parse-u-unicode (+ i 2))]
               [(char=? e #\U)       (parse-U-unicode (+ i 2))]
               [(char=? e #\newline) (parse-string (+ i 2))]
               [(char-oct? e)        (parse-octal (+ i 1))]
               [else
                (error 'parse-string-lexeme "illegal escape sequence" l)])])]
         [else (cons c (parse-string (+ i 1)))])]))
  (define (parse-octal i)
    ; we know that l[i] is an octal
    ; longer sequences take precedence
    (cond
      [(and (< (+ i 2) n)
            (char-oct? (string-ref l (+ i 1))) (char-oct? (string-ref l (+ i 2))))
       (cons (string->number (substring3 i) 8)
             (parse-string (+ i 3)))]
      [(and (< (+ i 1) n)
            (char-oct? (string-ref l (+ i 1))))
       (cons (string->number (substring2 i) 8)
             (parse-string (+ i 2)))]
      [else
       (cons (string->number (substring1 i) 8)
             (parse-string (+ i 1)))]))
  (define (parse-x-unicode i)
    (cond
      [(and (< (+ i 1) n)
            (char-hex? (string-ref l i)) (char-hex? (string-ref l (+ i 1))))
       (cons (integer->char (string->number (substring2 i) 16))
             (parse-string (+ i 2)))]
      [(and (< i n)
            (char-hex? (string-ref l i)))
       (cons (integer->char (string->number (substring1 i) 16))
             (parse-string (+ i 1)))]
      [else
       (error 'parse-string-lexeme
              "illegal string literal - \\x detected without any trailing hexadecimal digits")]))
  (define (parse-u-unicode i)
    ; the initial \u has been skipped
    ; either  \u<digit_16>^{4,4}
    ; or      \u<digit_16>^{4,4}\u<digit_16>^{4,4}
    (cond
      [(and (< (+ i 10) n)
            (char-hex? (string-ref l    i))
            (char-hex? (string-ref l (+ i 1)))
            (char-hex? (string-ref l (+ i 2)))
            (char-hex? (string-ref l (+ i 3)))
            (char=?    (string-ref l (+ i 4)) #\\)
            (char=?    (string-ref l (+ i 5)) #\u)
            (char-hex? (string-ref l (+ i 6)))
            (char-hex? (string-ref l (+ i 7)))
            (char-hex? (string-ref l (+ i 8)))
            (char-hex? (string-ref l (+ i 9))))
       (define hi (string->number (substring4    i)    16))
       (define lo (string->number (substring4 (+ i 6)) 16))
       (cons (utf-16-surrogate-pair->char hi lo)
             (parse-string (+ i 10)))]
      [(and (< (+ i 4) n)
            (char-hex? (string-ref l    i))
            (char-hex? (string-ref l (+ i 1)))
            (char-hex? (string-ref l (+ i 2)))
            (char-hex? (string-ref l (+ i 3))))
       (cons (integer->char (string->number (substring4 i) 16))
             (parse-string (+ i 4)))]
      [else
       (error 'parse-u-unicode
              "illegal string literal - \\u incorrectly used")]))
  (define (parse-U-unicode i) '(implement-me))
  (list->string (parse-string 0)))

(module+ test
  (require rackunit)
  (define (control s) (read (open-input-string (string-append "\"" s "\""))))
  (define plain-words '("" "a" "ab" "abc" "abcd" "abcde"))
  (define escapes     '("x\\ay" "x\\by" "x\\ty" "x\\ny" "x\\vy" "x\\fy" "x\\ry" "x\\ey"
                                "x\\\"y" "x\\'y" "x\\\\ny"))
  (define octals      '("\0" "\1" "\00" "\10" "\11" "\000" "\010" "\011" "\200" "\210" "\213"))
  (define hexes       '("\x0" "\x1" "\x00" "\x10" "\x11"
                              "a\x0b" "a\x1b" "a\x00b" "a\x10b" "a\x11b"
                              "7\x08" "7\x18" "7\x008" "7\x108" "7\x118"))
  (define unis       '("\u1" "\u12" "\u123" "\u1234" "\u12345"))
  (define suros      '("\u1\u2" "\u12\u34" "\u123\u456" "\u1234\u5678" "\u1234\u123455"))
  (define unis2      '("\U1" "\U12" "\U123" "\U1234" "\U12345" "\U100000"))
  (define others     '("a\\\'b" "a\\\"b" "a\\\\b"))
  (check-equal? (map string-lexeme->string plain-words) plain-words)
  (check-equal? (map string-lexeme->string escapes)     (map control escapes))
  (check-equal? (map string-lexeme->string octals)      (map control octals))
  (check-equal? (map string-lexeme->string hexes)       (map control hexes))
  (check-equal? (map string-lexeme->string unis)        (map control unis))
  (check-equal? (map string-lexeme->string suros)       (map control suros))
  (check-equal? (map string-lexeme->string unis2)       (map control unis2))
  (check-equal? (map string-lexeme->string others)      (map control others)))

 