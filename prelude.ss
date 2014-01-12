;; Our by-now usual prelude.

(define (compiler-error . bobbins)
  (error bobbins))

(define (runtime-error . bobbins)
  (error bobbins))

;; A unique value for uninitialised variables.
(define undefined-value '(constant . undefined))

(define (init* self . fields)
  (define (init1 fields)
    (if (pair? fields)
        (if (pair? (cdr fields))
            (begin
              ((car fields) self (cadr fields))
              (init1 (cddr fields)))
            (error "Field spec not in format (:mutator! value ...)" fields))))
  (init1 fields))
