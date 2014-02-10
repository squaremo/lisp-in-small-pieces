;; This compiler supports macros, by doing lexical analysis to convert
;; a program to objects (called 'Objectification' in the book). Macros
;; are expanded during this process. The result can then be
;; interpreted, compiled, disassembled, or whatever. The REPL given,
;; and the test hook, use the hosted eval.

(load "prelude.ss")
(import type-system)
(import generic-procedures)
(import oo)

(define-generics :name :name!)

;; Represents a binding
(define-class (<variable>)
  (name :name :name!))


;; A lexical environment. This abstract class is used as a marker.
(define-generics :next :next!)
(define-class (<environment>)
  (next :next :next!))
(define-method (initialize (<environment> self)
                           (<environment> next))
  (init* self :next! next))

;; The lexical scope; instead of a map, it's a chain of variable
;; definitions.
(define-generics :variable :variable!)
(define-class (<full-environment> <environment>)
  (next :next :next!)
  (variable :variable :variable!))
(define-method (initialize (<full-environment> self)
                           (<environment> next)
                           (<variable> variable))
  (init* self :next! next :variable! variable))

;; The first big change from previous compilers is that we use objects
;; to represent syntactic elements.

;; Abstract class representing an executable expression
(define-class (<program>))

;; A variable used to refer to a value
(define-class (<reference> <program>)
  (variable :variable :variable!))
(define-method (initialize (<reference> self)
                           (<variable> variable))
  (init* self :variable! variable))

(define-class (<global-variable> <variable>))

(define-generics :dotted? :dotted! :mutable? :mutable!)
(define-class (<local-variable> <variable>)
  (mutable :mutable? :mutable!)
  (dotted :dotted? :dotted!))
(define-method (initialize (<local-variable> self)
                           (<symbol> name)
                           (<boolean> mutable)
                           (<boolean> dotted))
  (init* self :name! name :mutable! mutable :dotted! dotted))

;; I could restrict these to the appropriate variable types, but meh.
(define-class (<local-reference> <reference>))
(define-class (<global-reference> <reference>))
(define-class (<predefined-reference> <reference>))


;; Global assignments use variables, local assignments use
;; references. Not totally sure why.
(define-generics :form :form!)
(define-class (<global-assignment> <program>)
  (variable :variable :variable!)
  (form :form :form!))
(define-method (initialize (<global-assignment> self)
                           (<global-variable> variable)
                           (<program> form))
  (init* self :variable! variable :form! form))

(define-generics :reference :reference!)
(define-class (<local-assignment> <program>)
  (reference :reference :reference!)
  (form :form :form!))
(define-method (initialize (<local-assignment> self)
                           (<local-variable> variable)
                           (<program> form))
  (init* self :variable! variable :form! form))

(define-generics :variables :variables!
  :body :body!)
(define-class (<function> <program>)
  (variables :variables :variables!)
  (body :body :body!))
(define-method (initialize (<function> self)
                           (<list> variables)
                           (<program> body))
  (init* self :variables! variables :body! body))

(define-generics :condition :condition!
  :consequent :consequent!
  :alternant :alternant!)
(define-class (<alternative> <program>)
  (condition :condition :condition!)
  (consequent :consequent :consequent!)
  (alternant :alternant :alternant!))
(define-method (initialize (<alternative> self)
                           (<program> condition)
                           (<program> consequent)
                           (<program> alternant))
  (init* self :condition! condition
         :consequent! consequent :alternant! alternant))

(define-generics :first :first! :last :last!)
(define-class (<sequence> <program>)
  (first :first :first!)
  (last :last :last!))
(define-method (initialize (<sequence> self)
                           (<program> first)
                           (<program> last))
  (init* self :first! first :last! last))

(define-generics :value :value!)
(define-class (<constant> <program>)
  (value :value :value!))
(define-method (initialize (<constant> self)
                           (<value> value))
  (init* self :value! value))

;; Abstract
(define-class (<application> <program>))

;; Abstract superclass for arguments, so we can be more specific than
;; <program> in applications.
(define-class (<arguments> <program>))

(define-generics :others :others!)

(define-class (<some-arguments> <arguments>)
  (first :first :first!)
  (others :others :others!))
(define-method (initialize (<some-arguments> self)
                           (<program> first)
                           (<arguments> others))
  (init* self :first! first :others! others))

(define-class (<no-argument> <arguments>))

(define-generics :function :function! :arguments :arguments!)
(define-class (<regular-application> <application>)
  (function :function :function!)
  (arguments :arguments :arguments!))
(define-method (initialize (<regular-application> self)
                           (<program> function)
                           (<arguments> arguments))
  (init* self :function! function :arguments! arguments))

(define-class (<description>))

(define-generics :description :description!)
(define-class (<predefined-variable> <variable>)
  (description :description :description!))
(define-method (initialize (<predefined-variable> self)
                           (<symbol> name)
                           (<description> description))
  (init* self :name! name :description! description))

(define-class (<predefined-application> <application>)
  (variable :variable :variable!)
  (arguments :arguments :arguments!))
(define-method (initialize (<predefined-application> self)
                           (<predefined-variable> variable)
                           (<arguments> arguments))
  (init* self :variable! variable :arguments! arguments))

(define-class (<fix-let> <program>)
  (variables :variables :variables!)
  (arguments :arguments :arguments!)
  (body :body :body!))
(define-method (initialize (<fix-let> self)
                           (<list> variables)
                           (<arguments> arguments)
                           (<program> body))
  (init* self :variables! variables
         :arguments! arguments :body! body))

(define-generics :comparator :comparator!
  :arity :arity! :generator :generator!)
(define-class (<functional-description> <description>)
  (comparator :comparator :comparator!)
  (arity :arity :arity!)
  (generator :generator :generator!))
(define-method (initialize (<functional-description> self)
                           (<procedure> comparator)
                           (<number> arity)
                           (<value> generator))
  (init* self :comparator! comparator :arity! arity :generator! generator))

;; This is going to represent anything that's a special form at the
;; current level of evaluation. There are some special forms in the
;; base language, and the rest will be macros.
(define-generics :handler :handler!)
;; I make magic-variable a subclass of variable, so it can go in
;; environments
(define-class (<magic-keyword> <variable>)
  (handler :handler :handler!))
(define-method (initialize (<magic-keyword> self)
                           (<symbol> name)
                           (<procedure> handler))
  (init* self :name! name :handler! handler))

;; =======================

;; OK now the stuff. This is the basic procedure to turn an
;; s-expression into some variety of <program>

;; As before, atom? isn't reliable, so I transpose this `if`
(define (objectify e r)
  (if (pair? e)
      (let ((m (objectify (car e) r)))
        (if (instance-of? m <magic-keyword>)
            ((:handler m) e r)
            (objectify-application m (cdr e) r)))
      (cond
       ;; not sure why it would *already* be objectified to a
       ;; magic-keyword
       ((instance-of? e <magic-keyword>) e)
       ((instance-of? e <program>) e)
       ((symbol? e) (objectify-symbol e r))
       (else (objectify-quotation e r)))))

(define (objectify-quotation e r)
  (make <constant> e))

(define (objectify-alternative ec et ef r)
  (make <alternative>
    (objectify ec r)
    (objectify et r)
    (objectify ef r)))

(define (objectify-sequence e* r)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (let ((a (objectify (car e*) r)))
            (make <sequence> a (objectify-sequence (cdr e*) r)))
          (objectify (car e*) r))
      (make <constant> 'undefined)))

(define (objectify-application ff e* r)
  (let ((ee* (->arguments (map (lambda (e) (objectify e r)) e*))))
    (cond ((instance-of? ff <function>)
           (process-closed-application ff ee*))
          ((instance-of? ff <predefined-reference>)
           (let* ((fvf (:variable ff))
                  (desc (:description fvf)))
             (if (instance-of? desc <functional-description>)
                 (if ((:comparator desc) (length e*) (:arity desc))
                     (make <predefined-application> fvf ee*)
                     (objectify-error
                      "Incorrect arity for predefined" ff e*))
                 (make <regular-application> ff ee*))))
          (else (make <regular-application> ff ee*)))))

(define (process-closed-application f e*)
  (let ((v* (:variables f))
        (b (:body f)))
    (if (and (pair? v*) (:dotted? (car (last-pair v*))))
        (process-nary-closed-application f e*)
        (if (= (length v*) (number-of e*))
            (make <fix-let> v* e* b)
            (objectify-error "Incorrect regular arity" f e*)))))

(define (->arguments e*)
  (if (pair? e*)
      (make <some-arguments> (car e*) (->arguments (cdr e*)))
      (make <no-argument>)))

(define-generics number-of)

(define-method (number-of (<no-argument> a))
  0)
(define-method (number-of (<some-arguments> a))
  (+ 1 (number-of (:others a))))


(define (last-pair l)
  (if (pair? l)
      (if (pair? (cdr l))
          (last-pair (cdr l))
          l)
      (error "This isn't even a pair.")))

(define (process-nary-closed-application f e*)
  (let* ((v* (:variables f))
         (b (:body f))
         (o (make <fix-let>
              v*
              (let gather ((e* e*) (v* v*))
                (if (:dotted? (car v*))
                    (make <some-arguments>
                      (let pack ((e* e*))
                        (if (instance-of? e* <some-arguments>)
                            ;; here we are making some code that will
                            ;; construct the varargs list
                            (make <predefined-application>
                              (find-variable? 'cons g.predef)
                              (make <some-arguments> (:first e*)
                                    (make <some-arguments> (pack (:others e*))
                                          (make <no-argument>))))
                            ;; I think this assumes e* is <no-argument>
                            (make <constant> '())))
                      (make <no-argument>))
                    ;; not dotted
                    (if (instance-of? e* <some-arguments>)
                        (make <some-arguments>
                          (:first e*)
                          (gather (:others e*)
                                  (cdr v*)))
                        ;; Again I think this assumes <no-argument>,
                        ;; meaning fewer args than there are
                        ;; non-varargs have been supplied.
                        (objectify-error "Incorrectly dotted arity"
                                         f e*))))
              ;; aaaaaand, include the body in the fix-let
              b)))
    ;; The last pair is the dotted bit. It's set to be 'normal',
    ;; because we've organised in this case to cons the list of
    ;; supplied arguments
    (:dotted! (car (last-pair v*)) #f)
    o))

(define (objectify-function names body r)
  (let* ((vars (objectify-variables-list names))
         (b (objectify-sequence body (r-extend* r vars))))
    (make <function> vars b)))

(define (objectify-variables-list names)
  (if (pair? names)
      (cons (make <local-variable> (car names) #f #f)
            (objectify-variables-list (cdr names)))
      (if (symbol? names)
          (list (make <local-variable> names #f #t))
          '())))

(define (objectify-symbol variable r)
  (let ((v (find-variable? variable r)))
    (cond ((instance-of? v <magic-keyword>) v)
          ((instance-of? v <local-variable>)
           (make <local-reference> v))
          ((instance-of? v <global-variable>)
           (make <global-reference> v))
          ((instance-of? v <predefined-variable>)
           (make <predefined-reference> v))
          (else (objectify-free-global-reference variable r)))))

;; Not sure of the justification of automagically creating globals
(define (objectify-free-global-reference name r)
  (let ((v (make <global-variable> name)))
    (insert-global! v r)
    (make <global-reference> v)))

;; NB 1: this doesn't get called above, but from the special form set!
;; NB 2: note that it annotates those variables that get mutated
(define (objectify-assignment variable e r)
  (let ((ov (objectify variable r))
        (of (objectify e r)))
    (cond ((instance-of? ov <local-reference>)
           (:mutable! (:variable ov) #t)
           (make <local-assignment> ov of))
          ((instance-of? ov <global-reference>)
           (make <global-assignment> (:variable ov) of))
          (else
           (objectify-error "Illegally mutated reference" variable)))))

;; ======= now environment stuff

(define (r-extend* r vars)
  (if (pair? vars)
      (r-extend (r-extend* r (cdr vars)) (car vars))
      r))

(define (r-extend r var)
  (make <full-environment> r var))

(define (insert-global! variable r)
  (let ((r (find-global-environment r)))
    (:next! r (make <full-environment> (:next r) variable))))

(define (find-global-environment r)
  (if (instance-of? r <full-environment>)
      (find-global-environment (:next r))
      r))

;; The start of the global environment is distinguished by a no-vars
;; environment
(define (mark-global-environment g)
  (make <environment> g))

(define (find-variable? name r)
  (if (instance-of? r <full-environment>)
      (let ((var (:variable r)))
        (if (eq? name (:name var))
            var
            (find-variable? name (:next r))))
      (if (instance-of? r <environment>)
          (find-variable? name (:next r))
          #f)))

;; ===== always-on special forms, that will be hooked into the env
;; later

(define special-if
  (make <magic-keyword>
    'if (lambda (e r)
          (objectify-alternative (cadr e) (caddr e) (cadddr e) r))))

(define special-begin
  (make <magic-keyword>
    'begin (lambda (e r)
             (objectify-sequence (cdr e) r))))

(define special-quote
  (make <magic-keyword>
    'quote (lambda (e r)
             (objectify-quotation (cadr e) r))))

(define special-set!
  (make <magic-keyword>
    'set! (lambda (e r)
            (objectify-assignment (cadr e) (caddr e) r))))

(define special-lambda
  (make <magic-keyword>
    'lambda (lambda (e r)
              (objectify-function (cadr e) (cddr e) r))))

(define *special-form-keywords*
  (list special-quote
        special-if
        special-begin
        special-set!
        special-lambda
        ;; book has let here too
        ))

;; Ohh there's a quick-and-dirty quasiquote in the book code

;; ========= Evaluators

;; Here, the book separates out levels of evaluation: macros, macros
;; that define macros, and so on -- we have to be prepared to expand a
;; macro that constructs another macro, or extends the environment.

;; There are a few hooks, in the form of generic procedures, that must
;; be defined later. One is `evaluate`, which will be used to
;; implement an interpreter for objectified programs (e.g.,
;; expanders); another is `invoke`, which is used to call a predefined
;; function during expansion.

(define-generics invoke evaluate)

(define-generics
  :mother :mother!
  :preparation :preparation!
  :runtime :runtime!
  :eval :eval!
  :expand :expand!)
(define-class (<evaluator>)
  (mother :mother :mother!)
  (preparation-environment :preparation :preparation!)
  (runtime-environment :runtime :runtime!)
  (eval :eval :eval!)
  (expand :expand :expand!))
(define-method (initialize (<evaluator> self)
                           (<evaluator> mother)
                           (<environment> prep)
                           (<environment> runtime)
                           (<procedure> eval)
                           (<procedure> expand))
  (init* self :mother! mother :preparation! prep :runtime! runtime
         :eval! eval :expand! expand))

(define-method (initialize (<evaluator> self)
                           (<value> mother) ;; <boolean> | <evaluator>
                           (<procedure> eval)
                           (<procedure> expand))
  (init* self :mother! mother :eval! eval :expand! expand
         :runtime! 'wait :preparation! 'wait))

;; This represents a predefined operation implemented in the host
;; language.
(define-generics :address :address!)
(define-class (<runtime-primitive>)
  (address :address :address!)
  (comparator :comparator :comparator!)
  (arity :arity :arity!))
(define-method (initialize (<runtime-primitive> self)
                           (<procedure> address)
                           (<procedure> comparator)
                           (<number> arity))
  (init* self :address! address :comparator! comparator :arity! arity))

;; This has a whole bunch of stuff that is interdependent; hence, the
;; `'wait`s.

(define (create-evaluator old)
  (let ((level 'wait)
        (g g.predef)
        (sg sg.predef))

    (define (expand e)
      (let ((prg (objectify e (:preparation level))))
        (enrich-with-new-global-variables! level)
        prg))

    (define (eval e)
      (let ((prg (expand e)))
        (evaluate prg (:runtime level))))

    (set! level (make <evaluator> old eval expand))

    ;; Special forms are always a part of the global env
    (set! g (r-extend* g *special-form-keywords*))
    (set! g (r-extend* g (make-macro-environment level)))

    ;; eval goes in the 
    (let ((eval-var (make <predefined-variable>
                      'eval (make <functional-description> = 1 "")))
          (eval-fn (make <runtime-primitive> eval = 1)))
      (set! g (r-extend g eval-var))
      (set! sg (sr-extend sg eval-var eval-fn)))

    (:preparation! level (mark-global-environment g))
    (:runtime! level (mark-global-runtime-environment sg))
    level))

;; Get new globals that appeared during expansion and make space for
;; them in the runtime environment
(define (enrich-with-new-global-variables! level)
  (let* ((g (find-global-environment (:preparation level)))
         (sg-head (find-global-runtime-environment (:runtime level))))
    (let loop ((g (:next g)))
      (when (instance-of? g <full-environment>)
            (let ((var (:variable g)))
              (when (and (instance-of? var <global-variable>)
                         (not (pair? (assq var sg-head))))
                    (set-cdr! sg-head (sr-extend (cdr sg-head) var undefined-value))))
            (loop (:next g))))))

;; Some predefined macros

;; Runs an expression in the next level up's evaluator
(define (special-eval-in-abbreviation-world evaluator)
  (lambda (e r)
    (let ((body (cdr e)))
      (objectify ((:eval (force evaluator))
                  `(,special-begin . ,body))
                 r))))

;; Insert a macro by creating a procedure using the next level up's eval.
(define (special-define-abbreviation evaluator)
  (lambda (e r)
    (let* ((call (cadr e))
           (body (cddr e))
           (name (car call))
           (variables (cdr body)))
      (let ((expander ((:eval (force evaluator))
                       `(,special-lambda ,variables ,body))))
        (define (handler e r)
          (objectify (invoke expander (cdr e)) r))
        (insert-global! (make <magic-keyword> name handler))))
    (objectify #t r)))

;; Insert macros in local scope by creating procedures using the next
;; level up's eval, and putting them in the env
(define (special-let-abbreviation evaluator)
  (lambda (e r)
    (let ((level (force evaluator))
          (macros (cadr e))
          (body (cddr e)))
      (define (make-macro def)
        (let* ((call (cadr def))
               (body (cddr def))
               (name (car call))
               (variables (cdr body)))
          (let ((expander ((:eval level)
                           `(,special-lambda ,variables ,body))))
            (define (handler e r)
              (objectify (invoke expander (cdr e)) r))
            (make <magic-keyword> name handler))))
      (objectify `(,special-begin . ,body)
                 (r-extend* r (map make-macro macros))))))

;; Reserve aliases for use in macros, by putting the runtime bindings
;; in the expansion environment of the level up, then continuing with
;; that level up.

(define (special-with-aliases evaluator)
  (lambda (e current-r)
    (let* ((level (force evaluator))
           (oldr (:preparation level))
           (oldsr (:runtime level))
           (aliases (cadr e))
           (body (cddr e)))
      (let bind ((aliases aliases)
                 (r oldr)
                 (sr oldsr))
        (if (pair? aliases)
            (let* ((variable (car (car aliases)))
                   (word (cadr (car aliases)))
                   (var (make <local-variable> variable #f #f)))
              (bind (cdr aliases)
                    (r-extend r var)
                    (sr-extend sr var (objectify word current-r))))
            (let ((result 'wait))
              (:preparation! level r)
              (:runtime! level sr)
              (set! result (objectify `(,special-begin . ,body) current-r))
              (:preparation! level oldr)
              (:runtime! level oldsr)
              result))))))

(define (make-macro-environment current)
  (let ((meta (delay (create-evaluator current))))
    (list (make <magic-keyword> 'eval-in-abbreviation-world
                (special-eval-in-abbreviation-world meta))
          (make <magic-keyword> 'define-abbreviation
                (special-define-abbreviation meta))
          (make <magic-keyword> 'let-abbreviation
                (special-let-abbreviation meta))
          (make <magic-keyword> 'with-aliases
                (special-with-aliases meta)))))

;; ==== OK now for our wee interpreter, and primitives for it to use

;; Escapes for errors while expanding
(define objectify-error 'wait)
(define evaluate-error 'wait)

;; Runtime environment, i.e., activation frames; this is used only in
;; the evaluator code.

(define (sr-extend* sr variables values)
  (if (pair? variables)
      (if (:dotted? (car variables))
          (sr-extend sr (car variables) values)
          (if (pair? values)
              (sr-extend (sr-extend* sr (cdr variables) (cdr values))
                         (car variables) (car values))
              (evaluate-error "Not enough values" variables)))
      (if (null? values)
          sr
          (evaluate-error "Too many values" values))))

(define (sr-extend sr var value)
  (cons (cons var value) sr))

;; If there's definitely no dotted var
(define (sr-regular-extend* sr variables values)
  (if (pair? variables)
      (if (pair? values)
          (sr-extend (sr-regular-extend* sr (cdr variables) (cdr values))
                     (car variables) (car values))
          (evaluate-error "Not enough values" variables))
      (if (null? values)
          sr
          (evaluate-error "Too many values" values))))

(define global-runtime-environment-mark (list (gensym)))
(define (mark-global-runtime-environment sg)
  (cons global-runtime-environment-mark sg))

(define (find-global-runtime-environment sg)
  (if (eq? (car sg) global-runtime-environment-mark)
      sg
      (find-global-runtime-environment (cdr sg))))

;; Our initial environments. The book starts these both at `'()`,
;; though g.predef ought to be some kind of <environment>.
(define g.predef (make <environment>))
;; The activation stack is just a list, though.
(define sg.predef '())

;; This represents a procedure in the object language
(define-generics :environment :environment!)
(define-class (<runtime-procedure>)
  (body :body :body!)
  (variables :variables :variables!)
  (environment :environment :environment!))
(define-method (initialize (<runtime-procedure> self)
                           (<program> body)
                           (<list> variables)
                           (<list> environment))
  (init* self :body! body :variables! variables :environment! environment))

(define-method (invoke (<runtime-procedure> f)
                       (<list> args))
  (unless (let check ((variables (:variables f))
                      (args args))
            (if (pair? variables)
                (or (:dotted? (car variables))
                    (and (pair? args)
                         (check (cdr variables) (cdr args))))
                (not (pair? args))))
          (evaluate-error "Wrong arity" f args))
  (evaluate (:body f)
            (sr-extend* (:environment f)
                        (:variables f)
                        args)))

;; And the interpreter itself: just recurses on `evaluate`
(define-method (evaluate (<local-reference> e) (<list> sr))
  (let ((v (assq (:variable e) sr)))
    (if (pair? v) (cdr v)
        (evaluate-error "Non-existant variable" e))))

(define-method (evaluate (<global-reference> e) (<list> sr))
  (let ((v (assq (:variable e) sr)))
    (if (pair? v)
        (let ((value (cdr v)))
          (if (eq? value undefined-value)
              (evaluate-error "Uninitialised variable" e)
              value))
        (evaluate-error "Non-existant variable" e))))

(define-method (evaluate (<global-assignment> e) (<list> sr))
  (let ((v (assq (:variable e) sr)))
    (if (pair? v)
        (let ((value (evaluate (:form e) sr)))
          (set-cdr! v value))
        (evaluate-error "Non-existant variable" e))))

(define-method (evaluate (<local-assignment> e) (<list> sr))
  (let* ((ref (:reference e))
         (v (assq (:variable ref) sr))
         (value (evaluate (:form e) sr)))
    (if (pair? v)
        (set-cdr! v value)
        (evaluate-error "Non-existant variable" e))))

(define-method (evaluate (<predefined-reference> e) (<list> sr))
  (cdr (assq (:variable e) sr)))

(define-method (evaluate (<function> e) (<list> sr))
  (make <runtime-procedure> (:body e) (:variables e) sr))

(define-method (evaluate (<alternative> e) (<list> sr))
  (if (evaluate (:condition e) sr)
      (evaluate (:consequent e) sr)
      (evaluate (:alternant e) sr)))

(define-method (evaluate (<sequence> e) (<list> sr))
  (begin (evaluate (:first e) sr)
         (evaluate (:last e) sr)))

(define-method (evaluate (<constant> e) (<list> sr))
  (:value e))

(define-method (evaluate (<regular-application> e)
                         (<list> sr))
  (let ((f (evaluate (:function e) sr))
        (args (evaluate (:arguments e) sr)))
    (invoke f args)))

(define-method (evaluate (<predefined-application> e)
                         (<list> sr))
  (let ((f (cdr (assq (:variable e) sr)))
        (args (evaluate (:arguments e) sr)))
    (invoke f args)))

(define-method (evaluate (<fix-let> e) (<list> sr))
  (let ((args (evaluate (:arguments e) sr)))
    (evaluate (:body e) (sr-regular-extend* sr (:variables e) args))))

(define-method (evaluate (<some-arguments> e) (<list> sr))
  (cons (evaluate (:first e) sr)
        (evaluate (:others e) sr)))

(define-method (evaluate (<no-argument> e) (<list> sr))
  '())

;; Here's the predefined procedures.

(define-method (invoke (<runtime-primitive> f)
                       (<list> args))
  (unless ((:comparator f) (length args) (:arity f))
          (evaluate-error "Wrong arity" f args))
  (apply (:address f) args))

;; Finally I'm going to go with syntax for these
(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (let ((v (make <predefined-variable> 'name (make <description>))))
       (set! g.predef (make <full-environment> g.predef v))
       (set! sg.predef (sr-extend sg.predef v value))
       'name))))

(define-syntax defprimitive
  (syntax-rules (>=0 >=2)
    ((defprimitive name value 0)
     (let ((v (make <predefined-variable> 'name
                    (make <functional-description> = 0 "")))
           (f (make <runtime-primitive> value = 0)))
       (set! g.predef (make <full-environment> g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value 1)
     (let ((v (make <predefined-variable> 'name
                    (make <functional-description> = 1 "")))
           (f (make <runtime-primitive> value = 1)))
       (set! g.predef (make <full-environment> g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value 2)
     (let ((v (make <predefined-variable> 'name
                    (make <functional-description> = 2 "")))
           (f (make <runtime-primitive> value = 2)))
       (set! g.predef (make <full-environment> g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value 3)
     (let ((v (make <predefined-variable> 'name
                    (make <functional-description> = 3 "")))
           (f (make <runtime-primitive> value = 3)))
       (set! g.predef (make <full-environment> g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value >=0)
     (let ((v (make <predefined-variable> 'name
                    (make <functional-description> >= 0 "")))
           (f (make <runtime-primitive> value >= 0)))
       (set! g.predef (make <full-environment> g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value >=2)
     (let ((v (make <predefined-variable> 'name
                    (make <functional-description> >= 2 "")))
           (f (make <runtime-primitive> value >= 2)))
       (set! g.predef (make <full-environment> g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))))

(definitial t #t)
(definitial f #f)
(definitial nil '())

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive atom? atom? 1)
(defprimitive symbol? symbol? 1)
(defprimitive null? null? 1)
(defprimitive not not 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive modulo modulo 2)
;(defprimitive display show 1)
(defprimitive newline newline 0)

(defprimitive list (lambda args args) >=0)

(defprimitive apply
  (lambda (f . args)
    (if (pair? args)
        (invoke f (let flat ((args args))
                    (if (null? (cdr args)) (car args)
                        (cons (car args) (flat (cdr args))))))
        (evaluate-error "Incorrect arity" 'apply)))
  >=2)

(defprimitive call/cc
  (lambda (f)
    (call/cc (lambda (k)
               (invoke f (list (make <runtime-primitive> k = 1))))))
  1)

;; Finally, a REPL

(define (repl)
  (set! objectify-error error)
  (set! evaluate-error error)
  (display "> ")
  (let ((in (read))
        (eval (:eval (create-evaluator #f))))
    (display (eval in))(newline)
    (repl)))

;; Hook for tests
(define (eval-expr e)
  ((:eval (create-evaluator #f)) e))
