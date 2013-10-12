#! /usr/bin/env scheme-r5rs

;; A test runner, effectively. Takes a file containing an interpreter
;; and outputs the results of evaluating the expressions in
;; "test-exprs.scm". Assumes a procedure `eval-exprs` which reads
;; expressions and evaluates them until it reaches EOF, displaying the
;; results to stdout.

;; Probably only works with SISC (http://sisc-scheme.org/) or
;; something with the same module system and modules, so e.g.,
;; (import type-system) and so on work.

;; Actually, other than those imports, it's all R5RS I think.

(define (main arguments)

  (load (cadr arguments))

  (with-input-from-port (open-input-file "test-exprs.scm")
    (lambda ()
      (define (eval-exprs)
        (let ((in (read)))
          (if (not (eq? #!eof in))
              (begin (display (eval-expr in))(newline)
                     (eval-exprs)))))
      (eval-exprs)))
  0)
