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

(load "prelude.ss")
(load "show.ss")

(max-stack-trace-depth 16)
#;(suppressed-stack-trace-source-kinds '())

(define (main arguments)

  (load (cadr arguments))

  (with-input-from-port (open-input-file "test-exprs.scm")
    (lambda ()
      (define (eval-exprs)
        (let* ((desc (read))
               (expr (read)))
          (when (not (eq? #!eof desc))
                (display desc)(newline)
                (with-failure-continuation
                 (lambda (e k)
                   (display e)(newline)
                   (eval-exprs))
                 (lambda () (display (eval-expr expr))(newline)))
                (eval-exprs))))
      (eval-exprs)))
  0)
