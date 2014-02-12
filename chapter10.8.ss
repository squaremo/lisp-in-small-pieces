;; Implements the compiler to C, using the AST-transforming of
;; chapter10.ss and runtime support.

(load "prelude.ss")
(load "chapter10.ss")

;; The book rejigs some of the environment management; in particular,
;; to put program-defined global variables in their own list. I want
;; to keep the structure from chapter9.ss, so I'm going to diverge a
;; bit on how globals are treated (by keeping it the same as before).

;; There are now two distinct phases: the primitives during
;; compilation differ from the primitives at runtime. To make things
;; simpler, I'm going to assume the set of primitives is the same.

(define (compile/C e out)
  (let* ((ev (create-evaluator #f))
         ;; Globals defined in the program get put in the environment
         ;; after the marker (a 'blank' environment) and before the
         ;; enhanced (with magic-keywords), predefined environment.
         (g.init (global-defs ev))
         (prg (-> e ((:expand ev)) transform))
         (g (collect-globals (global-defs ev) g.init)))
    (generate-C-program out e prg g)))

;; Helper for finding the "top" of the global environment in an
;; evaluator
(define (global-defs ev)
  (-> ev :preparation find-global-environment :next))

(define (generate-C-program out e p g)
  (generate-header out e)
  (generate-global-environment out g)
  (generate-quotations out (:quotations p))
  (generate-functions out (:definitions p))
  (generate-main out (:form p))
  (generate-trailer out)
  #;p)

(define (generate-header out e)
  (format out "/* Compiler to C ~%")
  (pretty-print e out)
  (format out "~%*/~%~%")
  (format out "#include \"scheme.h\"~%~%"))
(define (generate-trailer out)
  (format out "/* End of generated code */~%"))

;; === Globals

(define (collect-globals r init)
  (let collect ((g r)
                (g* '()))
    (if (equal? g init) g*
        (collect (:next g) (cons (:variable g) g*)))))

(define (generate-global-environment out g*)
  (for-each (lambda (gv)
              (generate-global-variable out gv)) g*))

(define (generate-global-variable out var)
  (let ((name (:name var)))
    (format out "SCM_DefineGlobalVariable(~A, \"~A\");~%"
            (IdScheme->IdC name) name)))

;; === Quotations

(define (generate-quotations out qv*)
  (when (pair? qv*)
        (format out "/* Quotations */~%")
        (scan-quotations out qv* (length qv*) '())))

(define (scan-quotations out qv* i results)
  (when (pair? qv*)
        (let* ((qv (car qv*))
               (value (:value qv))
               (other-qv (already-seen-value? value results)))
          (cond (other-qv
                 (generate-quotation-alias out qv other-qv)
                 (scan-quotations out (cdr qv*) i (cons qv results)))
                ((C-value? value)
                 (generate-C-value out qv)
                 (scan-quotations out (cdr qv*) i (cons qv results)))
                ((symbol? value)
                 (scan-symbol out value qv* i results))
                ((pair? value)
                 (scan-pair out value qv* i results))))))

(define (already-seen-value? v qv*)
  (and (pair? qv*)
       (if (equal? v (:value (car qv*))) (car qv*)
           (already-seen-value? v (cdr qv*)))))

;; (Remember that the name of a quotation variable is an index)
(define (generate-quotation-alias out q1 q2)
  (format out "#define thing~A thing~A /* ~S */~%"
          (:name q1) (:name q2) (:value q2)))

;; Immediate values, that is values that can directly represented in C

;; After the book, we'll use 16-bit fixnums
(define *max-fixnum* 16384)
(define *min-fixnum* (- *max-fixnum*))

(define (C-value? v)
  (or (null? v)
      (boolean? v)
      (and (integer? v)
           (< *min-fixnum* v *max-fixnum*))
      (string? v)))
(define (generate-C-value out qv)
  (let ((value (:value qv))
        (index (:name qv)))
    (cond ((null? value)
           (format out "#define thing~A SCM_nil /* () */~%" index))
          ((boolean? value)
           (format out "#define thing~A ~A /* ~S */~%" index
                   (if value "SCM_true" "SCM_false") value))
          ((integer? value)
           (format out "#define thing~A SCM_Int2fixnum(~A)~%"
                   index value))
          ((string? value)
           (format out "SCM_DefineString(thing~A_object, \"~A\");~%"
                   index value)
           (format out "#define thing~A SCM_wrap(&thing~A_object)~%"
                   index index)))))

;; Make a symbol out of an existing string
(define (scan-symbol out value qv* i results)
  (let* ((qv (car qv*))
         (str (symbol->string value))
         (strqv (already-seen-value? str results)))
    (cond (strqv (generate-symbol out qv strqv)
                 (scan-quotations out (cdr qv*) i (cons qv results)))
          (else
           (let ((newqv (make <quotation-variable> i str)))
             (scan-quotations out (cons newqv qv*) (+ i 1) results))))))

(define (generate-symbol out qv strqv)
  (format out "SCM_DefineSymbol(thing~A_object, thing~A); /* ~S */~%"
          (:name qv) (:name strqv) (:value qv))
  (format out "#define thing~A SCM_wrap(&thing~A_object)~%"
          (:name qv) (:name qv)))

(define (scan-pair out value qv* i results)
  (let* ((qv (car qv*))
         (d (cdr value))
         (dqv (already-seen-value? d results)))
    (if dqv
        (let* ((a (car value))
               (aqv (already-seen-value? a results)))
          (if aqv
              (begin
                (generate-pair out qv aqv dqv)
                (scan-quotations out (cdr qv*) i (cons qv results)))
              (let ((newaqv (make <quotation-variable> i a)))
                (scan-quotations out (cons newaqv qv*) (+ i 1) results))))
        ;; cdr not seen
        (let ((newdqv (make <quotation-variable> i d)))
          (scan-quotations out (cons newdqv qv*) (+ i 1) results)))))

(define (generate-pair out qv aqv dqv)
  (format out
          "SCM_DefinePair(thing~A_object, thing~A, thing~A) /* ~S */~%"
          (:name qv) (:name aqv) (:name dqv) (:value qv))
  (format out "#define thing~A SCM_wrap(&thing~A_object)~%"
          (:name qv) (:name qv)))


;; === Programs

(define-generics ->C)

(define <output-port> <character-output-port>)


(define-method (->C (<program> p) (<output-port> out))
  (error (list "->C unimplemented for " (class-name (type-of p)))))

;; Lots of things will need parens to disambiguate
(define-syntax in-parens
  (syntax-rules ()
    ((_ out . body)
     (let ((out out))
       (format out "(")
       (begin . body)
       (format out ")")))))

;; References and variables get compiled, in general, to C variables.
(define-method (->C (<reference> ref) (<output-port> out))
  (reference->C (:variable ref) out))

;; default implementations; there's two layers of specialisation.
(define-generics reference->C variable->C)
(define-method (reference->C (<variable> v) (<output-port> out))
  (variable->C v out))
(define-method (variable->C (<variable> v) (<output-port> out))
  (format out (IdScheme->IdC (:name v))))

(define-method (variable->C (<renamed-variable> v) (<output-port> out))
  (format out "~A_~A" (IdScheme->IdC (:name v)) (:index v)))
(define-method (variable->C (<quotation-variable> v) (<output-port> out))
  (format out "thing~A" (:name v)))

(define-method (reference->C (<global-variable> v) (<output-port> out))
  (format out "SCM_CheckedGlobal")
  (in-parens out (variable->C v out)))

(define-method (->C (<free-reference> ref) (<output-port> out))
  (format out "SCM_Free")
  (in-parens out (variable->C (:variable ref) out)))

;; Assignments: to globals, and box writes

(define-method (->C (<global-assignment> e) (<output-port> out))
  (in-parens out
             (variable->C (:variable e) out)
             (format out "=")
             (->C (:form e) out)))

(define-method (->C (<box-read> r) (<output-port> out))
  (format out "SCM_Content")
  (in-parens out (->C (:reference r) out)))

(define-method (->C (<box-write> w) (<output-port> out))
  (format out "SCM_Content")
  (in-parens out (->C (:reference w) out))
  (format out "=")
  (->C (:form w) out))

(define-method (->C (<box-creation> c) (<output-port> out))
  (variable->C (:variable c) out)
  (format out "= SCM_allocate_box")
  (in-parens out (variable->C (:variable c) out)))

;; If

(define-generics boolean->C)

(define-method (->C (<alternative> a) (<output-port> out))
  ;; The condition must be coerced to a boolean
  (in-parens out (boolean->C (:condition a) out)
             (format out "?~%")
             (->C (:consequent a) out)
             (format out ":~%")
             (->C (:alternant a) out)))

(define-method (boolean->C (<program> e) (<output-port> out))
  (in-parens out (->C e out)
             (format out " != SCM_false")))

;; Sequences

(define-method (->C (<sequence> seq) (<output-port> out))
  (in-parens out (->C (:first seq) out)
             (format out ",~%")
             (->C (:last seq) out)))

;; === Function applications
;; Here is where it gets fun.

(define-method (->C (<regular-application> a) (<output-port> out))
  (let ((n (number-of (:arguments a))))
    (cond ((< n 4)
           (format out "SCM_invoke~A" n)
           (in-parens out
                      (->C (:function a) out)
                      (->C (:arguments a) out)))
          (else
           (format out "SCM_invoke")
           (in-parens out
                      (->C (:function a) out)
                      (format out ",~A")
                      (->C (:arguments a) out))))))

(define-method (->C (<fix-let> f) (<output-port> out))
  (in-parens out
             (bindings->C (:variables f) (:arguments f) out)
             (->C (:body f) out)))

(define-generics bindings->C)
(define-method (bindings->C (<list> vars)
                            (<some-arguments> args)
                            (<output-port> out))
  (variable->C (car vars) out)(format out "=")
  (->C (:first args) out)(format out ",~%")
  (bindings->C (cdr vars) (:others args) out))
(define-method (bindings->C (<list> vars)
                            (<no-argument> _)
                            (<output-port> out))
  (format out "")) ;; Ummmmm

;; predefineds get inlined
(define-method (->C (<predefined-application> a) (<output-port> out))
  (-> a :variable runtime-description :generator
      (apply (list a out))))

;; For use by the inline generators. Note the second call, not to
;; arguments->C but to ->C
(define-generics arguments->C)
(define-method (arguments->C (<some-arguments> args) (<output-port> out))
  (->C (:first args) out)
  (->C (:others args) out))
(define-method (arguments->C (<no-argument> _) (<output-port> out))
  #t)
(define-method (->C (<some-arguments> args) (<output-port> out))
  (format out ",~%")
  (->C (:first args) out)
  (->C (:others args) out))
(define-method (->C (<no-argument> _) (<output-port> out))
  #t)

;; === Defining primitives

(define-method (initialize (<functional-description> self)
                           (<procedure> comp)
                           (<number> arity)
                           (<procedure> gen))
  (init* self :comparator! comp :arity! arity :generator! gen))


(define g.runtime '())

(define (runtime-description v)
  (let find ((g* g.runtime))
    (if (pair? g*)
        (let ((g (car g*)))
          (if (eq? (:name g) (:name v))
              (:description g)
              (find (cdr g*))))
        (compiler-error '("Predefined not found" (:name v))))))

(define-syntax def-rt-primitive
  (syntax-rules ()
    ((_ name Cname arity)
     (let ((v (make <predefined-variable> 'name
                    (make <functional-description> = arity
                          (make-predef-generator 'Cname)))))
       (set! g.runtime (cons v g.runtime))
       'name))))

(define (make-predef-generator Cname)
  (lambda (e out)
    (format out "~A" Cname)
    (in-parens out (arguments->C (:arguments e) out))))

(def-rt-primitive cons "SCM_cons" 2)
(def-rt-primitive car "SCM_car" 1)
(def-rt-primitive + "SCM_Plus" 2)
(def-rt-primitive = "SCM_EqnP" 2)

;; === Creating functions

(define-method (->C (<closure-creation> c) (<output-port> out))
  (format out "SCM_close")
  (in-parens out
             (format out "SCM_CfunctionAddress(function_~A), ~A, ~A"
                     (:index c)
                     (generate-arity (:variables c))
                     (number-of (:free c)))
             (->C (:free c) out)))

(define-method (number-of (<no-free> _)) 0)
(define-method (number-of (<some-free> f))
  (+ 1 (number-of (:others f))))

(define (generate-arity vars)
  (let count ((vars vars) (arity 0))
    (if (pair? vars)
        (if (:dotted? (car vars))
            (- (+ arity 1))
            (count (cdr vars) (+ 1 arity)))
        arity)))

(define-method (->C (<no-free> _) (<output-port> out))
  #t)
(define-method (->C (<some-free> f) (<output-port> out))
  (format out ",~%")
  (->C (:first f) out)
  (->C (:others f) out))

;; === Function definitions

(define (generate-functions out definitions)
  (format out "~%/* Functions */~%")
  (for-each (lambda (def)
              (generate-closure-structure out def)
              (generate-possibly-dotted-def out def))
            (reverse definitions)))

(define (generate-closure-structure out def)
  (format out "SCM_DefineClosure(function~A, "
          (:index def))
  (generate-local-temporaries out (:free def))
  (format out ");~%"))

;; NB book has inconsistent order of args here. I've corrected it.
(define (generate-possibly-dotted-def out def)
  (format out "~%SCM_DeclareFunction(function_~A) {~%"
          (:index def))
  (let ((vars (:variables def))
        (rank -1))
    ;; this is kind of a gross way to do it
    (for-each (lambda (v)
                (set! rank (+ rank 1))
                (cond ((:dotted? v)
                       (format out "SCM_DeclareDottedVariable("))
                      ((instance-of? v <variable>)
                       (format out "SCM_DeclareVariable(")))
                (variable->C v out)
                (format out ",~A);~%" rank))
              vars)
    (let ((temps (:temporaries def)))
      (when (pair? temps)
            (generate-local-temporaries out temps)
            (format out "~%")))
    (format out "return ")
    (->C (:body def) out)
    (format out ";~%}~%~%")))

(define (generate-local-temporaries out temps)
  (when (pair? temps)
        (format out "SCM ")
        (variable->C (car temps) out)
        (format "; ")
        (generate-local-temporaries (cdr temps) out)))

;; === Main

(define (generate-main out form)
  (format out "~%/* Expression: */~%")
  (format out "void main(void) {~%")
  (format out "  SCM_print")
  (in-parens out (->C form out))
  (format out ";~%  exit(0);~%}~%"))


;; === Converting Scheme names to C names

;; I've just copied this from the book code, as you'll be able to tell
;; from the funny paren-style
(define Scheme->C-names-mapping
  '( (*        . "TIMES")
     (+        . "PLUS")
     (-        . "DIFFERENCE")
     (/        . "QUOTIENT")
     (>        . "GREATERP")
     (>=       . "NOT_LESSP")
     (<        . "LESSP")
     (<=       . "NOT_GREATERP")
     (=        . "EQN")
     (eq?      . "EQ")
     (pair?    . "CONSP")
     (symbol?  . "SYMBOLP")
     (set-car! . "RPLACA")
     (set-cdr! . "RPLACD")
     ) )

(define (IdScheme->IdC name)
  (let ((v (assq name Scheme->C-names-mapping)))
    (if (pair? v) (cdr v)
        (let ((str (symbol->string name)))
          (let retry ((Cname (compute-Cname str)))
            (if (Cname-clash? Cname Scheme->C-names-mapping)
                (retry (compute-another-Cname str))
                (begin (set! Scheme->C-names-mapping
                             (cons (cons name Cname)
                                   Scheme->C-names-mapping ) )
                       Cname ) ) ) ) ) ) )

(define (Cname-clash? Cname mapping)
  (let check ((mapping mapping))
    (and (pair? mapping)
         (or (string=? Cname (cdr (car mapping)))
             (check (cdr mapping)) ) ) ) )

;;; These functions compute a C name for a symbol. Scheme symbols
;;; cannot be transleted into a name containing an isolated underscore
;;; so all these names will be used for C generation purposes.

(define compute-another-Cname
  (let ((counter 1))
    (lambda (str)
      (set! counter (+ 1 counter))
      (compute-Cname (format #f "~A_~A" str counter)) ) ) )

(define (compute-Cname str)
  (define (mapcan f l)
    (if (pair? l)
        (append (f (car l)) (mapcan f (cdr l)))
        '() ) )
  (define (convert-char char)
    (case char
      ((#\_)             '(#\_ #\_))
      ((#\?)             '(#\p))
      ((#\!)             '(#\i))
      ((#\<)             '(#\l))
      ((#\>)             '(#\g))
      ((#\=)             '(#\e))
      ((#\- #\/ #\* #\:) '())
      (else              (list char)) ) )
  (let ((cname (mapcan convert-char (string->list str))))
    (if (pair? cname) (list->string cname) "weird") ) )