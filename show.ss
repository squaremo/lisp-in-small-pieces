;; Friendly representations of classes (and values in general, but
;; most just `show` to themselves)

(import type-system)
(import oo)

(define (show v)
  (cond ((or (instance-of? v <symbol>)
             (instance-of? v <number>)
             (instance-of? v <boolean>)
             (instance-of? v <string>)
             (instance-of? v <char>)
             (instance-of? v <procedure>)
             (eq? v (void)))
         v)
        ((list? v)
         (map show v))
        ((pair? v) ;; catch dotted pairs
         (cons (show (car v)) (show (cdr v))))
        ((vector? v)
         (list->vector (map show (vector->list v))))
        (else
         (show-class-value v))))

(define (show-class-value v)
  (let collect ((classes (class-precedence-list (type-of v)))
                (slots '()))
    (if (null? classes)
        `(,(class-name (type-of v))
          ,@(map (lambda (s) (show-class-slot s v)) slots))
        (collect (cdr classes)
                 (append slots (class-direct-slots (car classes)))))))

(define symbol->list (compose string->list symbol->string))
(define list->symbol (compose string->symbol list->string))
(define (string-prepend a b) (string-append b a))

(define (show-slot-name s)
  (-> s slot-name symbol->string (string-prepend ":") string->symbol))

(define (show-class-slot s v)
  `(,(show-slot-name s) ,(show ((slot-accessor s) v))))
