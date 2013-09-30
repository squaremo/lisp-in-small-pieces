;; Our by-now usual prelude.

(define (compiler-error . bobbins)
  (error bobbins))

(define (runtime-error . bobbins)
  (error bobbins))

;; A unique value for uninitialised variables.
(define undefined-value '(constant . undefined))
