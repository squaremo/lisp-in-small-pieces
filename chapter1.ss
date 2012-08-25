;; I'm using SISC 1.16.6 and C-u M-x run-scheme sisc

;; We'll keep our implementation primitives prefixed with #$, to make
;; it easy to distinguish them.

;; What's wrong? Don't be like that.

(define ($wrong . args)
  (error args))

;; Environment. As per the book we used an improper (cons cells rather
;; than lists) alist.  We also use symbols as the keys, which saves us
;; transforming from symbols to variable names to lookup keys (i.e.,
;; we implicitly lift var names from the interpreted language to
;; symbols in the interpreting language).

(define $env.init '())

(define ($lookup key env)
  (if (pair? env)
      (if (eq? (caar env) key)
          (cdar env)
          ($lookup key (cdr env)))
      ($wrong "Env lookup failed" key)))

(define ($update! key env value)
  (if (pair? env)
      (if (eq? caar env)
          (begin (set-cdr! (car env) value)
                 value)
          ($update! key (cdr env) value))
      ($wrong "No such binding" key)))

(define ($extend variables values env)
  (cond ((pair? variables)
         (if (pair? values)
             ;; grr not tail call
             (cons (cons (car variables) (car values))
                   ($extend (cdr variables) (cdr values) env))
             ($wrong "Too few values" variables)))
        ((null? variables)
         (if (null? values)
             env
             ($wrong "Too many values" values)))
        ;; dotted
        ((symbol? variables) (cons (cons variables values) env))))

;; Ground values

(define $false 'FALSE)
(define $undef 'UNDEFINED)

;; Heeelpers -- not bothered with distinguishing these.

(define (any-of expr predicates)
  (if (pair? predicates)
      (or ((car predicates) expr)
          (any-of expr (cdr predicates)))
      #f))

(define (eval-args args env)
  (if (pair? args)
      ;; rely on in-order eval of interpreting language
      ;; not tail recursive, ugh
      (cons ($evaluate (car args) env) (eval-args (cdr args) env))
      '()))

;; Evaluate

(define ($evaluate expr env)
  ;; WTF: with SISC, (atom? (quote f)) => #f
  (if (pair? expr)
      ;; a form!
      (case (car expr)
        ((quote) (cadr expr))
        ((if) (if (not (eq? $FALSE ($evaluate (cadr expr) env)))
                  ($evaluate (caddr expr) env)
                  ($evaluate (cadddr expr))) env)
        ((begin) ($eprogn (cdr expr) env))
        ((set!) ($update! (cadr expr) ($evaluate (caddr expr) env)))
        ((lambda) ($make-function (cadr expr) (cddr expr) env))
        (else ($invoke ($evaluate (car expr) env)
                       (eval-args (cdr expr) env))))
      ;; an atom
      (cond
        ((symbol? expr)
         ($lookup expr env))
        ((any-of expr (list number? string? char? boolean? vector?))
         expr)
        (else
          ($wrong "Cannot evaluate atom" expr)))))

(define ($eprogn exprs env)
  (if (pair? exprs)
      (let ((rest (cdr exprs)))
        (if (pair? rest)
            (begin
              ($evaluate (car exprs) env)
              ($eprogn rest env))
            ($evaluate (car exprs) env)))
      $undef))

(define ($make-function variables body env)
  (lambda values ;; A departure: rather than accepting an argument
                 ;; that is the list of arguments, expect to be
                 ;; applied to a list of arguments. This makes lifting
                 ;; and dropping procedures easier.
    ($eprogn body ($extend variables values env))))

(define ($invoke fn values)
  (if (procedure? fn)
      (apply fn values)
      ($wrong "Not a function" fn)))

;; ... and that's our strict, lexically-scoped interpreter.

;; Easy way to run it:

(define $env.global $env.init)

(define ($def name val)
  (set! $env.global ($extend name val $env.global)))

(define (comparison fn) (lambda (vals) (if (fn vals) #t $false)))
;; This is kind of vestigial, but I may want this later if application
;; changes.
(define ($fn fn) fn)

($def '< (comparison <))
($def '> (comparison >))
($def 'eq? (comparison eq?))
($def '+ ($fn +))
($def '- ($fn -))

(define (repl)
  (define (toplevel)
    (let ((expr (read)))
      (if (eq? '(exit) expr)
          (display "Exiting")
          (begin
            (display ($evaluate (read) $env.global))
            (toplevel)))))
  (toplevel))

;; Exercise 1.1

;; Modify to trace function invocations
(define ($invoke-trace fn args)
  (let ((result (fn args)))
    (display args)(display " => ")(display result)(newline)
    result))

(define $invoke-notrace $invoke)
(set! $invoke $invoke-trace)

;; undo ..
(set! $invoke $invoke-notrace)

;; Exercise 1.2

;; Rewrite eval-args to avoid additional recursion
;; (this is the case everywhere, oh well)

(define (eval-args exprs env)
  (if (null? exprs)
      '()
      (if (pair? (cdr exprs))
          (cons ($evaluate (car exprs) env)
                (eval-exprs (cdr exprs) env))
          (list ($evaluate (car exprs) env)))))

;; From the book's answer: the outer test only needs to be done once.
(define (eval-args exprs env)
  (define (evargs exprs)
    ;; can assume (pair? exprs)
    (if (pair? (cdr exprs))
        (cons ($evaluate (car exprs) env)
              (evargs (cdr exprs)))
        (list ($evaluate (car exprs) env))))
  (if (pair? exprs)
      (evargs exprs)
      '()))

;; Exercise 1.3

;; If we define this:
(define ($extend names values env)
  (cons (cons names values) env))

;; Now reimplement lookup and update!

(define ($lookup key env)

  (define (lookup* key names values)
    (if (null? names)
        ($lookup key (cdr env))
        (if (eq? (car names) key)
            (car values)
            (lookup* key (cdr names) (cdr values)))))
  
  (if (null? env)
      ($wrong "Lookup failed" key)
      (lookup* key (caar env) (cdar env))))

(define ($update key value env)

  (define (update* key names values cell)
    (if (null? names)
        ($update key value (cdr env))
        (if (eq? (car names) key)
            (set-cdr! cell (cons value (cdr values)))
            (update* key (cdr names) (cdr values) values))))

  (if (null? env)
      ($wrong "Variable not found" key)
      (update* key (caar env) (cdar env) (car env))))

;; Exercise 1.6

;; Define list

($def 'list ($fn list))
;; NB harder if you used the syntax provided in the book, which checks
;; arity of defined functions. One would have to do syntax that
;; avoided that.

;; Exercise 1.7 implement call/cc

(define ($evaluate expr env)
  (if (pair? expr)
      ;; a form!
      (case (car expr)
        ((quote) (cadr expr))
        ((if) (if (not (eq? $FALSE ($evaluate (cadr expr) env)))
                  ($evaluate (caddr expr) env)
                  ($evaluate (cadddr expr))) env)
        ((begin) ($eprogn (cdr expr) env))
        ((set!) ($update! (cadr expr) ($evaluate (caddr expr) env)))
        ((lambda) ($make-function (cadr expr) (cddr expr) env))
        ;; OK. Here we have to both lift the interpreted target into a
        ;; meta-lambda, but drop the supplied continuation to an
        ;; object lambda. Luckily, these look the same, since we made
        ;; our object-lambdas applicable in exactly the same way as
        ;; meta-lambdas.
        ((call/cc) (let ((target ($evaluate (cadr expr) env)))
                     (call/cc (lambda (k)
                                ($invoke target (list k))))))
        (else ($invoke ($evaluate (car expr) env)
                       (eval-args (cdr expr) env))))
      ;; an atom
      (cond
        ((symbol? expr)
         ($lookup expr env))
        ((any-of expr (list number? string? char? boolean? vector?))
         expr)
        (else
          ($wrong "Cannot evaluate atom" expr)))))

;; Exercise 1.8

;; Define apply

($def 'apply ($fn apply))
;; book answer has more complication, because of its apply convention,
;; and syntax for defining things. Technically I ought to do more
;; argument checking; I leave it to the interpreting language.
