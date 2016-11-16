#lang racket/base
(provide #%infix)
(require racket/stxparam
         (for-syntax racket/base))
(define-syntax-parameter #%infix
  (λ (stx)
    (syntax-case stx ()
      [(_ expr) #'expr])))