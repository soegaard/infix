#lang racket
(require scribble/eval)

(provide the-eval)

(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require "../main.rkt"))
    the-eval))