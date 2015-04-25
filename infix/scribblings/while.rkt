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
     #'(let loop () (when expr body ... (loop)))]))
