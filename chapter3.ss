;; Outside world

(define ($wrong . args)
  (error args))

;; Roughly equivalent to CLOS (based on tinyclos?)
;; http://sisc-scheme.org/manual/html/ch07.html#GenericProcedures

(import type-system)
(import generic-procedures)
(import oo)

;; Just to keep it compact. These are the drivers.
(define-generics
  $evaluate ;; expression environment continuation
  $invoke ;; function values environment continuation
  $resume ;; continuation values
  $lookup ;; environment name continuation
  $update!) ;; environment name value continuation

;; For now I want to keep the convenient s-expression as code, but I
;; want to mark where it's used as a distinct type.
(define <expression> <value>)

;; For $lookup and $update! to dispatch on. This lets us use dispatch
;; to distinguish among different states of environment (empty, not
;; empty, basically). In principal it also lets us supply a different
;; implementation of environment, using e.g., setprop/getprop or
;; thread locals.
(define-class (<environment>))

;; A superclass for things that are invokable, so I can be specific in
;; type signatures.
(define-class (<invokable>))

;; (SISC makes us declare accessors and mutators, so there will be a
;; lot of these.)
(define-generics :k :k!)

(define (init* self . fields)
  (define (init1 fields)
    (if (pair? fields)
        (if (pair? (cdr fields))
            (begin
              ((car fields) self (cadr fields))
              (init1 (cddr fields)))
            (error "Field spec not in format (:mutator! value ...)" fields))))
  (init1 fields))

;; For dispatching on 'what happens next'. This could just be a
;; closure, resumed by applying it to the value.  This will also serve
;; as our 'outermost' continuation (bottom-continuation in the book),
;; if given a native procedure.
(define-class (<continuation> <invokable>) (k :k :k!))
(define-method (initialize (<continuation> self)
                           (<procedure> k))
  (init* self :k! k))
(define-method ($resume (<continuation> self) (<value> val))
  ((:k self) val))

;; For the proper error
(define-method ($invoke (<value> v)
                        (<list> _args)
                        (<environment> _env)
                        (<continuation> _k))
  ($wrong "Attempt to invoke non-function" v))

;; Environments

(define-generics :value :value! :name :name! :others :others!)

(define-class (<null-env> <environment>))

(define-class (<full-env> <environment>)
  (others :others :others!)
  (name :name :name!))
(define-method (initialize (<full-env> env)
                           (<environment> others)
                           (<symbol> name))
  (init* env :others! others
             :name! name))

(define-class (<var-env> <full-env>)
  (value :value :value!))
(define-method (initialize (<var-env> env)
                           (<environment> others)
                           (<symbol> name)
                           (<value> value))
  (init* env :others! others :name! name :value! value))

(define (extend-env env names values)
  (cond ((and (pair? names) (pair? values))
         (make <var-env> (extend-env env (cdr names) (cdr values))
               (car names) (car values)))
        ((and (null? names) (null? values)) env)
        ((symbol? names) (make <var-env> env names values))
        (else ($wrong "Arity mismatch" names values))))

;; Really not sure why there's full-env and var-env, with a different
;; defn of lookup or update! for full-env -- when will they be
;; invoked?

(define-method ($lookup (<null-env> _empty)
                        (<symbol> name)
                        (<continuation> k))
  ($wrong "Unknown variable" name))
(define-method ($lookup (<full-env> env)
                        (<symbol> name)
                        (<continuation> k))
  ($lookup (:others env) name k))
(define-method ($lookup (<var-env> env)
                        (<symbol> name)
                        (<continuation> k))
  (if (eqv? (:name env) name)
      ($resume k (:value env))
      ($lookup (:others env) name k)))

(define-method ($update! (<null-env> env)
                         (<symbol> name)
                         (<value> value)
                         (<continuation> k))
  ($wrong "Unknown variable" name value k))
(define-method ($update! (<full-env> env)
                         (<symbol> name)
                         (<value> value)
                         (<continuation> k))
  ($update! (:others env) name value k))
(define-method ($update! (<var-env> env)
                         (<symbol> name)
                         (<value> value)
                         (<continuation> k))
  (if (eqv? name (:name env))
      (begin
        (:value! env value)
        ($resume k value))
      ($update! (:others env) name value k)))

;; Interpreter 'loop'

;; For now, don't specialise on the type of expression.
(define-method ($evaluate (<expression> e)
                          (<environment> env)
                          (<continuation> k))
  (if (pair? e)
      (case (car e)
        ((quote) (evaluate-quote (cadr e) env k))
        ((if) (evaluate-if (cadr e) (caddr e) (cadddr e) env k))
        ((begin) (evaluate-begin (cdr e) env k))
        ((set!) (evaluate-set! (cadr e) (caddr e) env k))
        ((lambda) (evaluate-lambda (cadr e) (cddr e) env k))
        (else (evaluate-apply (car e) (cdr e) env k)))
      
      (cond ((symbol? e) (evaluate-variable e env k))
            (else (evaluate-quote e env k)))))

;; quoted or literal

(define (evaluate-quote v env k)
  ($resume k v))

;; variable ref
(define (evaluate-variable name env k)
  ($lookup env name k))

;; If

(define-generics :s-exp :s-exp! :f-exp :f-exp! :env :env!)

(define-class (<if-k> <continuation>)
  (s-exp :s-exp :s-exp!)
  (f-exp :f-exp :f-exp!)
  (env :env :env!))
(define-method (initialize (<if-k> self)
                           (<continuation> k)
                           (<expression> success)
                           (<expression> fail)
                           (<environment> env))
  (init* self :k! k
              :s-exp! success
              :f-exp! fail
              :env! env))

(define (evaluate-if c-exp s-exp f-exp env k)
  ($evaluate c-exp env (make <if-k> k s-exp f-exp env)))

(define-method ($resume (<if-k> k) (<value> v))
  ($evaluate (if v (:s-exp k) (:f-exp k))
             (:env k)
             (:k k)))

;; Begin

(define-generics :exprs :exprs!)

(define-class (<begin-k> <continuation>)
  (env :env :env!)
  (exprs :exprs :exprs!))
(define-method (initialize (<begin-k> self)
                           (<continuation> k)
                           (<list> exprs)
                           (<environment> env))
  (init* self :k! k :exprs! exprs :env! env))

(define (evaluate-begin exprs env k)
  (if (pair? exprs)
      (if (pair? (cdr exprs))
          ;; not tail call
          ($evaluate (car exprs) env (make <begin-k> k exprs env))
          ($evaluate (car exprs) env k))
      ;; nothing to do here. walk on.
      ($resume k $undef)))

(define-method ($resume (<begin-k> k) (<value> v))
  (evaluate-begin (cdr (:exprs k)) (:env k) (:k k)))

;; set!

(define-class (<set-k> <continuation>)
  (name :name :name!)
  (env :env :env!))
(define-method (initialize (<set-k> self)
                           (<continuation> k)
                           (<symbol> name)
                           (<environment> env))
  (init* self :k! k :name! name :env! env))

(define-method ($resume (<set-k> k) (<value> v))
  ($update! (:env k) (:name k) v (:k k)))

(define (evaluate-set! name expr env k)
  ($evaluate expr env (make <set-k> k name env)))

;; lambda

(define-generics :variables :variables!)

;; NB It's <procedure> that's predefined in SISC's type system, so
;; we're not shadowing anything. Also: <object> <= <value>, and we're
;; punning object-level values with meta-level values (e.g.,
;; object-level numbers are represented with meta-level numbers).
(define-class (<function> <invokable>)
  (variables :variables :variables!)
  (exprs :exprs :exprs!)
  (env :env :env!))
(define-method (initialize (<function> self)
                           (<list> variables)
                           (<list> exprs)
                           (<environment> env))
  (init* self :variables! variables
              :exprs! exprs
              :env! env))

(define (evaluate-lambda variables body env k)
  ($resume k (make <function> variables body env)))

(define-method ($invoke (<function> f)
                        (<list> values)
                        (<environment> env)
                        (<continuation> k))
  (let ((env1 (extend-env (:env f) (:variables f) values)))
    (evaluate-begin (:exprs f) env1 k)))

;; Here is where the fun starts.

;; There are a number of continuations here:

;; This is a cheat so we only have to define the init once.
(define-class (<fun-k> <continuation>)
  (exprs :exprs :exprs!)
  (env :env :env!))
(define-method (initialize (<fun-k> self)
                           (<continuation> k)
                           (<list> exprs)
                           (<environment> env))
  (init* self :k! k :exprs! exprs :env! env))

;; Evaluated the head, let's get the args
(define-class (<fun-head-k> <fun-k>))

;; Evaluating an argument
(define-class (<arg-k> <fun-k>))

(define-class (<cons-k> <continuation>)
  (value :value :value!))
(define-method (initialize (<cons-k> self)
                           (<continuation> k)
                           (<value> value))
  (init* self :k! k :value! value))

;; Head and args evaluated, ready to apply
(define-class (<apply-k> <continuation>)
  (value :value :value!) ;; the function to apply
  (env :env :env!))
(define-method (initialize (<apply-k> self)
                           (<continuation> k)
                           (<invokable> fun)
                           (<environment> env))
  (init* self :k! k :value! fun :env! env))

;; Here's where we might also check if the head is invokable.
(define-method ($resume (<fun-head-k> k)
                        (<invokable> fn))
  (evaluate-args (:exprs k) (:env k)
                      (make <apply-k> (:k k) fn (:env k))))

(define-method ($resume (<arg-k> k)
                        (<value> v))
  (evaluate-args (cdr (:exprs k)) (:env k)
                      (make <cons-k> (:k k) v)))

(define-method ($resume (<cons-k> k) (<list> vs))
  ($resume (:k k) (cons (:value k) vs)))

(define-method ($resume (<apply-k> k) (<list> args))
  ($invoke (:value k) args (:env k) (:k k)))

(define (evaluate-apply head body env k)
  ($evaluate head env (make <fun-head-k> k body env)))

(define (evaluate-args exprs env k)
  (if (pair? exprs)
      ($evaluate (car exprs) env (make <arg-k> k exprs env))
      ($resume k '())))

;; Wowsers, that's it for the interpreter. Now for some prims.

(define-class (<primitive> <function>)
  (name :name :name!)
  (value :value :value!))
(define-method (initialize (<primitive> self)
                           (<symbol> name)
                           (<procedure> fn))
  (init* self :name! name :value! fn))

(define-method ($invoke (<primitive> p)
                        (<list> args)
                        (<environment> env)
                        (<continuation> k))
  ((:value p) args env k))

(define (prim fn)
  (lambda (vs env k)
    ($resume k (apply fn vs))))

(define env.global (make <null-env>))

(set! env.global
  (let ((fns '(+ - < >)))
    (extend-env env.global
                fns
                (map (lambda (fn)
                       (make <primitive> fn (prim (eval fn))))
                     fns))))

(set! env.global
  (extend-env env.global '(call/cc)
              (list (make <primitive> 'call/cc
                          (lambda (vs env k)
                            ($invoke (car vs) (list k) env k))))))

(define-method ($invoke (<continuation> fn)
                        (<list> args)
                        (<environment> env)
                        (<continuation> k))
  ($resume fn (car args)))
