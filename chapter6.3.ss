;; This interpreter is adapted from the one in chapter6.ss.
;;
;; In §6.2 (which I'm skipping), the interpreter from §6.1 is changed
;; to use a global register for its environment and to treat tail
;; calls differently (since they need not restore the environment,
;; thus keeping constant stack).
;;
;; In the §6.3 interpreter we'll remove the continuation-passing from
;; the interpreter, and instead use call/cc in the host language to
;; implement call/cc in the interpreted language. The combination of a
;; register for the environment and direct style means forms are now
;; compiled -- sorry, pretreated -- to thunks, moving towards a more
;; VM-like result (do some stuff to registers, jump).

;; Our by-now usual prelude.

(define (compiler-error . bobbins)
  (error bobbins))

(define (runtime-error . bobbins)
  (error bobbins))

(import type-system)
(import generic-procedures)
(import oo)


;; Activation records: I'm collapsing the frankly profligate two
;; classes (environment, not used qua itself, and activation) from the
;; previous interpreter into just the one. We're still using the
;; nested environments.

;; I'm exercising the type system a little more than the book does,
;; just by defining some procedures as generic procedures for the
;; effect of checking the arguments passed match the signature given.

(define-generics
  :next :next!
  :args :args!
  :argument :argument!
  :length
  extend
  deep-fetch deep-update!)

(define-class (<activation>)
  (next :next :next!)
  (args :args :args!))

(define-method (initialize (<activation> self)
                           (<number> size))
  (:args! self (make-vector size)))

(define-method (:argument (<activation> frame)
                          (<number> index))
  (vector-ref (:args frame) index))

(define-method (:argument! (<activation> frame)
                           (<number> index)
                           (<value> value))
  (vector-set! (:args frame) index value))

(define-method (:length (<activation> self))
  (vector-length (:args self)))

(define-method (extend (<activation> parent)
                       (<activation> child))
  (:next! child parent)
  child)

(define-method (deep-fetch (<activation> sr)
                           (<number> level)
                           (<number> index))
  (if (= level 0)
      (:argument sr index)
      (deep-fetch (:next sr) (- level 1) index)))

(define-method (deep-update! (<activation> sr)
                             (<number> level)
                             (<number> index)
                             (<value> value))
  (if (= level 0)
      (:argument! sr index value)
      (deep-update! (:next sr) (- level 1) index value)))

;; Closures are back to being objects rather than erm, closures.

(define-generics :code :code! :closed-env :closed-env!)

(define-class (<closure>)
  (code :code :code!)
  (closed-env :closed-env :closed-env!))

(define-method (initialize (<closure> self)
                           (<procedure> code)
                           (<activation> closed))
  (:code! self code)
  (:closed-env! self closed))

;; I'm going to use a generic procedure here, as well, again for the
;; effect of checking the type of argument it's given.
(define-generics invoke)

(define-method (invoke (<closure> f)
                       (<activation> v*))
  ((:code f) v* (:closed-env f)))

;; Lexical environment: exactly as it was previously

(define (r-extend* r n*)
  (cons n* r))

;; See if the given name is a local variable in the given environment
(define (local-variable? r i n)
  (and (pair? r)
       (let scan ((names (car r))
                  (j 0))
         (cond ((pair? names)
                (if (eq? n (car names))
                    `(local ,i . ,j)
                    (scan (cdr names) (+ j 1))))
               ((null? names)
                (local-variable? (cdr r) (+ i 1) n))
               ;; Don't think I understand this clause -- why would
               ;; these be improper? A convenience perhaps
               ((eq? n names) `(local ,i . ,j))))))

;; Names of mutable globals
(define g.current '())
;; Names of predefined globals
(define g.init '())

;; Mutable globals
(define sg.current (make-vector 100))
;; Predefined globals
(define sg.init (make-vector 100))

;; Initial env
(define r.init '())
;; Initial memory
(define sr.init (make <activation> 0))

;; A unique value for uninitialised variables. I made the name all
;; uppercase in the previous interpreter, but that would clash with
;; the combinator names here.
(define undefined-value '(constant . undefined))

;; Frame pointer
(define *env* undefined-value)

(define (g.current-extend! n)
  (let ((level (length g.current)))
    (set! g.current (cons (cons n `(global . ,level)) g.current))
    level))

(define (g.init-extend! n)
  (let ((level (length g.init)))
    (set! g.init (cons (cons n `(predefined . ,level)) g.init))
    level))

(define (compute-kind r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n)))

(define (global-variable? g n)
  (let ((var (assq n g)))
    (and (pair? var) (cdr var))))

(define (global-fetch i)
  (vector-ref sg.current i))
(define (predef-fetch i)
  (vector-ref sg.init i))

(define (global-update! i v)
  (vector-set! sg.current i v))

(define (g.current-init! name)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((global)
           (global-update! (cdr kind) UNDEFINED))
          (else
            (compiler-error "Bad redefinition" name kind)))
        (let ((index (g.current-extend! name)))
          (global-update! index UNDEFINED))))
  name)

(define (g.init-init! name value)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((predefined)
           (vector-set! sg.init (cdr kind) value))
          (else (compiler-error "Bad redefinition" name kind)))
        (let ((index (g.init-extend! name)))
          (vector-set! sg.init index value))))
  name)

(define (define-initial name value)
  (g.init-init! name value))

;; Primitives

(define desc.init '())

(define (description-extend! name description)
  (set! desc.init (cons (cons name description) desc.init))
  name)

(define (get-description name)
  (let ((d (assq name desc.init)))
    (and (pair? d) (cdr d))))


;; Right then.

;; This actually comes from the previous section §6.2, and isn't
;; changed in §6.3. All the procedures to which it delegates do
;; change, of course.
(define (meaning e r tail?)
  (if (pair? e)
      (case (car e)
        ((quote) (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if) (meaning-alternative (cadr e) (caddr e) (cadddr e)
                                   r tail?))
        ((begin) (meaning-sequence (cdr e) r tail?))
        ((set!) (meaning-assignment (cadr e) (caddr e) r tail?))
        (else (meaning-application (car e) (cdr e) r tail?)))
      (if (symbol? e)
          (meaning-deref e r tail?)
          (meaning-quotation e r tail?))))

;; Literal values and quotations

(define (meaning-quotation v r tail?)
  (CONSTANT v))

(define (CONSTANT v)
  (lambda () v))

;; Variable references

(define (meaning-deref n r tail?)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((level (cadr kind))
                 (index (cddr kind)))
             (if (= level 0)
                 (SHALLOW-ARGUMENT-REF index)
                 (DEEP-ARGUMENT-REF level index))))
          ((global)
           (let ((index (cdr kind)))
             (CHECKED-GLOBAL-REF index)))
          ((predefined)
           (let ((index (cdr kind)))
             (PREDEFINED index))))
        (compiler-error "No such variable" n))))

(define (SHALLOW-ARGUMENT-REF index)
  (lambda () (:argument *env* index)))

(define (DEEP-ARGUMENT-REF level index)
  (lambda () (deep-fetch *env* level index)))

(define (CHECKED-GLOBAL-REF index)
  (lambda ()
    (let ((v (global-fetch index)))
      (if (eq? v undefined-value)
          (runtime-error "Uninitialised variable")
          v))))

(define (PREDEFINED index)
  (lambda () (predef-fetch index)))

;; Conditional

(define (meaning-alternative ec et ef r tail?)
  (let ((mc (meaning ec r #f))
        (mt (meaning et r tail?))
        (mf (meaning ef r tail?)))
    (ALTERNATIVE mc mt mf)))

(define (ALTERNATIVE mc mt mf)
  (lambda () (if (mc) (mt) (mf))))

;; Assignment

(define (meaning-assignment n e r tail?)
  (let ((m (meaning e r #f))
        (kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((level (cadr kind))
                 (index (cddr kind)))
             (if (= level 0)
                 (SHALLOW-ASSIGNMENT! index m)
                 (DEEP-ASSIGNMENT! level index m))))
          ((global)
           (let ((index (cdr kind)))
             (GLOBAL-SET! index m)))
          ((predefined)
           (compiler-error
            "Attempted to assign to immutable variable" n)))
        (compiler-error "Unknown variable" n))))

(define (SHALLOW-ASSIGNMENT index m)
  (lambda () (:argument! *env* index (m))))

(define (DEEP-ASSIGNMENT level index m)
  (lambda () (deep-update! *env* level index (m))))

(define (GLOBAL-SET! index m)
  (lambda () (global-update! index (m))))

;; Begin

(define (meaning-sequence e+ r tail?)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r tail?)
          (meaning*-single-sequence (car e+) r tail?))
      (compiler-error "Illegal form (begin)")))

(define (meaning*-single-sequence e r tail?)
  (meaning e r tail?))

(define (meaning*-multiple-sequence e e* r tail?)
  (let ((m (meaning e r #f))
        (m+ (meaning-sequence e* r tail?)))
    (SEQUENCE m m+)))

(define (SEQUENCE m m+)
  (lambda () (m) (m+)))

;; OK now the slightly harder bits. Starting with

;; Abstraction

(define (meaning-abstraction nn* e+ r tail?)
  (let parse ((n* nn*)
              (regular '()))
    (cond ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
          ;; We ran through them all and no dot!
          ;; Use nn* to avoid having to reverse `regular`
          ((null? n*) (meaning-fix-abstraction nn* e+ r tail?))
          (else (meaning-dotted-abstraction
                 (reverse regular) n* e+ r tail?)))))

(define (meaning-fix-abstraction n* e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2 #t)))
    (FIX-CLOSURE m+ arity)))

(define (meaning-dotted-abstraction n* n e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 #t)))
    (NARY-CLOSURE m+ arity)))

(define (FIX-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      ;; The book defines a named procedure here instead of inlining a
      ;; lambda. As I'm intrigued by this, I'm going to do it too.
      (define (the-function v* sr) ;; sr = closed-over environment
        ;; the v* here will have been freshly consed for this invocation,
        ;; so we can happily mutate it
        (if (= (:length v*) arity+1)
            (begin (set! *env* (extend sr v*))
                   (m+))
            (runtime-error "Incorrect arity for procedure"
                           "expected" arity
                           "got" (- (:length v*) 1))))
      (make <closure> the-function *env*))))

(define (listify! v* arity)
  (let loop ((index (- (vector-length (:args v*)) 1))
             (result '()))
    (if (= arity index)
        (:argument! v* arity result)
        (loop (- index 1)
              (cons (:argument v* (- index 1)) result)))))

(define (NARY-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (>= (:length v*) arity+1)
            (begin
              (listify! v* arity)
              (set! *env* (extend sr v*))
              (m+))
            (runtime-error "Incorrect arity"
                           "expected at least" arity
                           "got" (- (:length v*) 1))))
      (make <closure> the-function *env*))))


;; The most fun of all, application

(define (meaning-application e e* r tail?)
  (cond ((and (symbol? e)
              (let ((kind (compute-kind r e)))
                (and (pair? kind)
                     (eq? 'predefined (car kind))
                     ;; As before I move the checking into
                     ;; meaning-primitive-application; it just gets to
                     ;; messy here.
                     (meaning-primitive-application e e* r tail?)))))
        ((and (pair? e)
              (eq? 'lambda (car e)))
         (meaning-closed-application e e* r tail?))
        (else (meaning-regular-application e e* r tail?))))

(define (meaning-regular-application e e* r tail?)
  (let ((m (meaning e r #f))
        (m* (meaning* e* r (length e*) #f)))
    (if tail?
        (TR-REGULAR-CALL m m*)
        (REGULAR-CALL m m*))))

(define (meaning* e* r size tail?)
  (if (pair? e*)
      (meaning-some-args (car e*) (cdr e*) r size tail?)
      (meaning-no-arg r size tail?)))

(define (meaning-no-arg r size tail?)
  (ALLOCATE-FRAME size))

(define (meaning-some-args e e* r size tail?)
  (let ((m (meaning e r tail?))
        (m* (meaning* e* r size tail?))
        (index (- size (+ (length e*) 1))))
    (STORE-ARGUMENT m m* index)))

(define (TR-REGULAR-CALL m m*)
  (lambda ()
    (invoke (m) (m*))))

(define (REGULAR-CALL m m*)
  (lambda ()
    (let* ((f (m))
           (v* (m*))
           (sr *env*) ;; get the env
           (result (invoke f v*))) ;; do the call
      (set! *env* sr) ;; restore the env
      result)))

(define (STORE-ARGUMENT m m* index)
  (lambda ()
    (let ((v (m))
          (v* (m*)))
      (:argument! v* index v)
      v*)))

(define (ALLOCATE-FRAME size)
  (let ((size+1 (+ size 1)))
    (lambda () (make <activation> size+1))))

;; left-left-lambda
;; ((lambda (n*...) body) ee*...)
(define (meaning-closed-application e ee* r tail?)
  (let parse ((n* (cadr e))
              (e* ee*)
              (regular '()))
    (cond
      ((pair? n*)
       (if (pair? e*)
           (parse (cdr n*) (cdr e*) (cons (car n*) regular))
           (compiler-error "Too few arguments: need" (cadr e)
                           "got" ee*)))
      ((null? n*)
       (if (null? e*)
           (meaning-fix-closed-application
            (cadr e) (cddr e) ee* r tail?)
           (compiler-error "Too many arguments: need" (cadr e)
                           "got" ee*)))
      (else
        (meaning-dotted-closed-application
         (reverse regular) n* (cddr e) ee* r tail?)))))

;; ((lambda (a b) (+ a b)) 1 2)
(define (meaning-fix-closed-application n* body e* r tail?)
  (let* ((m* (meaning* e* r (length e*) #f))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2 tail?)))
    (if tail?
        (TR-FIX-LET m* m+)
        (FIX-LET m* m+))))

;; ((lambda as (apply + as)) 1 2 3)
(define (meaning-dotted-closed-application n* n body e* r tail?)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*) #f))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2 tail?)))
    (if tail?
        (TR-FIX-LET m* m+)
        (FIX-LET m* m+))))

(define (meaning-dotted* e* r size arity tail?)
  (if (pair? e*)
      (meaning-some-dotted-args (car e*) (cdr e*)
                                r size arity tail?)
      (meaning-no-dotted-arg r size arity tail?)))

(define (meaning-some-dotted-args e e* r size arity tail?)
  (let ((m (meaning e r tail?))
        (m* (meaning-dotted* e* r size arity tail?))
        (index (- size (+ (length e*) 1))))
    (if (< index arity)
        (STORE-ARGUMENT m m* index)
        (CONS-ARGUMENT m m* arity))))

(define (meaning-no-dotted-arg r size arity tail?)
  (ALLOCATE-DOTTED-FRAME arity))

(define (TR-FIX-LET m* m+)
  (lambda ()
    (set! *env* (extend *env* (m*)))
    (m+)))

(define (FIX-LET m* m+)
  (lambda ()
    (set! *env* (extend *env* (m*)))
    (let ((result (m+)))
      (set! *env* (:next *env*))
      result)))

(define (CONS-ARGUMENT m m* arity)
  (lambda ()
    (let ((v (m))
          (v* (m*)))
      (:argument! v* arity (cons v (:argument v* arity)))
      v*)))

;; Because it's a dotted lambda, we know to pre-load the last argument
;; slot with an empty list, for the rest args.
(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (let ((v* (make <activation> arity+1)))
        (:argument v* arity '())
        v*))))


(define (meaning-primitive-application e e* r tail?)
  (let ((desc (get-description e)))
    (and desc ;; I don't know why it wouldn't be there, but anyway
         ;; desc = (function address . arity)
         (or (eq? 'function (car desc))
             (compiler-error "Function expected"))
         (let ((address (cadr desc))
               (size (caddr desc)))
           (and
            ;; I did say I would check arity here
            (or (= size (length e*))
                (compiler-error "Wrong arity for " e
                                "expected" size))
            ;; This time I'll do it the book way; this sets up some of the VM
            ;; instructions later on.
            (case size
              ((0) (CALL0 address))
              ((1) (let ((m (meaning (car e*) r #f)))
                     (CALL1 address m)))
              ((2) (let ((m1 (meaning (car e*) r #f))
                         (m2 (meaning (cadr e*) r #f)))
                     (CALL2 address m1 m2)))
              ((3) (let ((m1 (meaning (car e*) r #f))
                         (m2 (meaning (cadr e*) r #f))
                         (m3 (meaning (caddr e*) r #f)))
                     (CALL3 address m1 m2 m3)))
              (else
                (meaning-regular-application e e* r tail?))))))))

(define (CALL0 address)
  (lambda () (address)))

(define (CALL1 address m1)
  (lambda () (address (m1))))

(define (CALL2 address m1 m2)
  (lambda ()
    (address (m1) (m2))))

(define (CALL3 address m1 m2 m3)
  (lambda ()
    ;; The book uses a let* here to ensure left-to-right evaluation, I
    ;; guess because otherwise it's undefined? I'm not so fussed,
    ;; anyway.
    (address (m1) (m2) (m3))))

;; This is more along the lines of the book, just translating into a
;; procedure rather than a macro (which I think is
;; unnecessary). Goodness there's a lot of repitition.
(define (define-primitive name underlying arity)
  (let ((arity+1 (+ arity 1)))
    (case arity
      ((0)
       (define-initial name
         (let ((behaviour
                (lambda (v* sr)
                  (if (= (:length v*) arity+1)
                      (underlying)
                      (runtime-error "Wrong number of arguments"
                                     "expected" 0
                                     "got" (- (:length v*) 1))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <closure> behaviour sr.init))))
      ((1)
       (define-initial name
         (let ((behaviour
                (lambda (v* sr)
                  (if (= (:length v*) arity+1)
                      (underlying (:argument v* 0))
                      (runtime-error "Wrong number of arguments"
                                     "expected" 1
                                     "got" (- (:length v*) 1))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <closure> behaviour sr.init))))
      ((2)
       (define-initial name
         (let ((behaviour
                (lambda (v* sr)
                  (if (= (:length v*) arity+1)
                      (underlying (:argument v* 0)
                                  (:argument v* 1))
                      (runtime-error "Wrong number of arguments"
                                     "expected" 2
                                     "got" (- (:length v*) 1))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <closure> behaviour sr.init))))
      ((3)
       (define-initial name
         (let ((behaviour
                (lambda (v* sr)
                  (if (= (:length v*) arity+1)
                      (underlying (:argument v* 0)
                                  (:argument v* 1)
                                  (:argument v* 2))
                      (runtime-error "Wrong number of arguments"
                                     "expected" 3
                                     "got" (- (:length v*) 1))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <closure> behaviour sr.init)))))))


;; We can now define apply and call/cc, both as initial variables
;; rather than primitives, so they get better access to the underlying
;; machinery.

(define-initial 'apply
  (let ((arity 2)
        (arity+1 3))
    (make <closure>
          (lambda (v* sr)
            ;; `apply` has a weird definition:
            ;; http://www.schemers.org/Documents/Standards/R5RS/HTML/
            ;; This is why we have to shuffle things around here,
            ;; rather than just expecting `(apply f args)`
            (if (>= (:length v*) arity+1)
                ;; Why does the book have `- 2` here? Because it's not
                ;; a varargs we're looking for, it's the last argument
                ;; supplied -- really, last-arg is
                ;; 'last-non-list-arg'. It's sort of a reverse
                ;; varargs.
                (let* ((f (:argument v* 0))
                       (last-arg-index (- (:length v*) 2))
                       (last-arg (:argument v* last-arg-index))
                       (size (+ last-arg-index (length last-arg)))
                       (frame (make <activation> size)))
                  (do ((i 1 (+ i 1)))
                      ((= i last-arg-index))
                    (:argument! frame (- i 1) (:argument v* i)))
                  (do ((i (- last-arg-index 1) (+ i 1))
                       (last-arg last-arg (cdr last-arg)))
                      ((null? last-arg))
                    (:argument! frame i (car last-arg)))
                  (invoke f frame))
                (runtime-error "Wrong number of arguments to apply"
                               "expected at least 2")))
          sr.init)))

(define-initial 'call/cc
  (let ((arity 1)
        (arity+1 2))
    (make <closure>
          (lambda (v* sr) ;; = (<f> to call with k, toplevel env)
            (if (= arity+1 (:length v*))
                (call/cc
                 (lambda (metak)
                   (let ((f (:argument v* 0))
                         ;; Make an activation frame for f, which
                         ;; we'll fill in presently
                         (frame (make <activation> 2)))
                     (:argument!
                      frame 0
                      (make <closure>
                            (lambda (v* _sr)
                              (if (= 2 (:length v*))
                                  (metak (:argument v* 0))
                                  (runtime-error
                                   "Wrong number of args"
                                   "to continuation")))
                            sr.init))
                     (invoke f frame))))
                (runtime-error "Wrong number of args"
                               "to call/cc")))
          sr.init)))

;; Finally, our repl

(define (repl)
  (define (toplevel)
    (set! *env* sr.init)
    (display "> ")
    (display ((meaning (read) r.init #t)))(newline)
    (toplevel))
  (toplevel))
