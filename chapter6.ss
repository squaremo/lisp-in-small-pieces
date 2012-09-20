;; In this interpreter, the static part of a program is explicitly
;; separated from the dynamic. Broadly speaking, the static are the
;; lexical environment and instructions, and the dynamic the
;; activation frames and continuation.
;;
;; Activation frames represent memory; that is, they store values
;; against addresses. The environment maps names to those addresses,
;; abstractly -- that is, we determine which activation frame will
;; have the memory address while compiling, and look it up at
;; runtime. The only representation of memory kept in this interpreter
;; is the activation records.

(define (compiler-error . bobbins)
  (error bobbins))

(define (runtime-error . bobbins)
  (error bobbins))

;; Environments and activation records. Both contain maps, and
;; activation records contain a link to the next record.

(import type-system)
(import generic-procedures)
(import oo)

(define-generics :next :next! :args :args! :argument :argument!)

(define-class (<environment>)
  (next :next :next!))

(define-class (<activation> <environment>)
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


;; Extend the activation frame (working memory, I guess)
(define (sr-extend* sr v*)
  (:next! v* sr)
  v*)

;; Extend the environment. This works slightly differently to the
;; activation records -- it's just a list of assoc lists. (Why?)
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
                (local-variable? (cdr r) (+ i 1)))
               ;; Don't think I understand this clause -- why would
               ;; these be improper? A convenience perhaps
               ((eq? n names) `(local ,i . ,j))))))

;; When we compile expressions, we replace variable references with
;; lookups into the activation records (that's i for the number of
;; frames up, and j for the slot). This is going to go retrieve the
;; values for us.
(define (deep-fetch sr i j)
  (if (= i 0)
      (:argument sr j)
      (deep-fetch (:next sr) (- i 1) j)))

;; Likewise for set!
(define (deep-update! sr i j value)
  (if (= i 0)
      (:argument! sr j value)
      (deep-update! (:next sr) (- i 1) j value)))

;; Global (top-level) variables: these are in two varieties, mutable
;; (defined by the program?) and immutable (primitives). They can be
;; shadowed of course, and we know this at interpretation time, so we
;; can insert the correct lookup.

(define (compute-kind r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n)))
(define (global-variable? g n)
  (let ((var (assq n g)))
    (and (pair? var) (cdr var))))

;; Global envs are just a list of (addr (global . name))
;; i.e., an assoc list

(define (g.current-extend! n)
  (let ((level (length g.current)))
    (set! g.current (cons (cons n `(global . ,level)) g.current))
    level))

(define (g.init-extend! n)
  (let ((level (length g.init)))
    (set! g.init (cons (cons n `(predefined . ,level)) g.init))
    level))

;; and global memory is just a vector
(define sg.current (make-vector 100))
(define sg.init (make-vector 100))

(define (global-fetch i)
  (vector-ref sg.current i))
(define (predef-fetch i)
  (vector-ref sg.init i))

(define (global-update! i v)
  (vector-set! sg.current i v))

;; Redefine or initialise a global variable (either predef'd or user)

;; NB ref to r.init -- ugly.

(define UNDEFINED '(constant . undefined))

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

;; OK now for real stuff.

;; `meaning` is the compilation or (as per the book) pretreatment
;; step. The idea is to create a lambda that, given the store
;; (activation records) and the continuation, will execute the
;; program.

;; I'm finally going to cede to the book way of naming variables, in
;; particular environments 'r'. (Presumably r for \rho from chapter 5)

(define (meaning e r)
  (if (pair? e)
      (case (car e)
        ((quote) (meaning-quotation (cadr e) r))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r))
        ((if) (meaning-alternative (cadr e) (caddr e) (cadddr e) r))
        ((begin) (meaning-sequence (cdr e) r))
        ((set!) (meaning-assignment (cadr e) (caddr e) r))
        (else (meaning-application (car e) (cdr e) r)))
      (if (symbol? e)
          (meaning-deref e r)
          (meaning-quotation e r))))

(define (meaning-quotation v r)
  (lambda (sr k) (k v)))

(define (meaning-alternative e1 e2 e3 r)
  (let ((m1 (meaning e1 r))
        (m2 (meaning e2 r))
        (m3 (meaning e3 r)))
    (lambda (sr k)
      (m1 sr (lambda (v)
               ((if v m2 m3) sr k))))))

(define (meaning-sequence e+ r)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r)
          (meaning*-single-sequence (car e+) r))
      (compiler-error "Empty begin")))

(define (meaning*-multiple-sequence e1 e+ r)
  (let ((m1 (meaning e1))
        (m+ (meaning-sequence e+ r)))
    (lambda (sr k)
      (m1 sr (lambda (v) (m+ sr k))))))
(define (meaning*-single-sequence e r)
  (meaning e r))


;; First tricky one: application. This makes us determine how
;; procedures are represented. (As per book, I'll just use a closure)

(define (meaning-application e e* r)
  (cond
    ((and (pair? e)
          (eq? 'lambda (car e)))
     (meaning-closed-application e e* r))
    (else
      (meaning-regular-application e e* r))))

(define (meaning-regular-application e e* r)
  (let* ((m (meaning e r))
         (m* (meaning* e* r (length e*)))) ;; pass length for size of
                                           ;; activation rec
    (lambda (sr k)
      (m sr (lambda (fn)
              (if (procedure? fn) ;; object-procedure = meta-procedure
                  (m* sr (lambda (v*)
                           (fn v* k)))
                  (runtime-error "Not a function" fn)))))))

;; "left left lambda"
(define (meaning-closed-application e ee* r)
  (let ((nn* (cadr e)))
    (let parse ((n* nn*)
                (e* ee*)
                (regular '()))
      (cond
        ((pair? n*)
         (if (pair? e*)
             (parse (cdr n*) (cdr e*) (cons (car n*) regular))
             (compiler-error "Too few arguments" e ee*)))
        ((null? n*)
         (if (null? e*)
             (meaning-fix-closed-application nn* (cddr e) ee* r)
             (compiler-error "Too many arguments" e ee*)))
        (else ;; augh, rest args in a let-ish form ..
          (meaning-dotted-closed-application
           (reverse regular) n* (cddr e) ee* r))))))

(define (meaning-fix-closed-application n* body e* r)
  (let* ((m* (meaning* e* r (length e*)))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2)))
    (lambda (sr k)
      (m* sr (lambda (v*)
               (m+ (sr-extend* sr v*) k))))))

(define (meaning-dotted-closed-application n* n body e* r)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*)))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2)))
    (lambda (sr k)
      (m* sr (lambda (v*)
               (m+ (sr-extend* sr v*) k))))))         

;; As the book says, because we know the number of arguments being
;; supplied, we can build the rest list as we go; essentially a
;; transformation of the 'excess' argument expressions from
;; r1 .. r2 .. r3 to (cons r1 (cons r2 (cons r3 '())))

(define (meaning-dotted* e* r size arity)
  (if (pair? e*)
      (meaning-some-dotted-args (car e*) (cdr e*) r size arity)
      (meaning-no-dotted-arg r size arity)))

(define (meaning-some-dotted-args e e* r size arity)
  (let ((m (meaning e r))
        (m* (meaning-dotted* e* r size arity))
        (rank (- size (length e*) 1)))
    (if (< rank arity) ;; if still in 'obligatory' arguments
        (lambda (sr k)
          (m sr (lambda (v)
                  (m* sr (lambda (v*)
                           (:argument! v* rank v)
                           (k v*))))))
        ;; else we're in rest args
        (lambda (sr k)
          (m sr (lambda (v)
                  (m* sr (lambda (v*)
                           (:argument! v* arity (cons v (:argument v* arity)))
                           (k v*)))))))))

(define (meaning-no-dotted-arg r size arity)
  (let ((arity+1 (+ arity 1)))
    (lambda (sr k)
      (let ((v* (make <activation> arity+1)))
        (:argument! v* arity '())
        (k v*)))))

;; Compile (a fixed number of) arguments. The continuation gets the
;; activation frame.
(define (meaning* e* r size)
  (if (pair? e*)
      (meaning-some-args (car e*) (cdr e*) r size)
      (meaning-no-args r size)))

;; Make an activation frame for each invocation (see book for
;; discussion)
(define (meaning-no-args r size)
  (let ((size+1 (+ 1 size)))
    (lambda (sr k)
      (let ((v* (make <activation> size+1)))
        (k v*)))))

(define (meaning-some-args e e* r size)
  (let ((m1 (meaning e r))
        (m* (meaning* e* r size))
        (index (- size (length e*) 1)))
    (lambda (sr k)
      (m1 sr (lambda (v)
               (m* sr (lambda (v*)
                        (:argument! v* index v)
                        (k v*))))))))

;; All the environment stuff above is now useful for compiling -- I
;; mean pretreating -- variable references and assignment.

(define (meaning-deref n r)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)))
             (if (= i 0)
                 (lambda (sr k)
                   (k (:argument sr j)))
                 (lambda (sr k)
                   (k (deep-fetch sr i j))))))
          ((global)
           (let ((i (cdr kind)))
             ;; This is of dubious utility -- only check later if it's
             ;; undefined now
             (if (eq? (global-fetch i) UNDEFINED)
                 (lambda (sr k)
                   (let ((value (global-fetch i)))
                     (if (eq? value UNDEFINED)
                         (runtime-error "variable not defined" n)))))))
          ((predefined)
           (let* ((i (cdr kind))
                  (value (predef-fetch i)))
             (lambda (sr k)
               (k value)))))
        (compiler-error "No variable" n))))

(define (meaning-assignment n e r)
  (let ((m (meaning e r))
        (kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)))
             (if (= i 0)
                 (lambda (sr k)
                   (m sr (lambda (val)
                           (k (:argument! sr j val)))))
                 (lambda (sr k)
                   (m sr (lambda (val)
                           (k deep-update! sr i j val)))))))
          ((global)
           (let ((i (cdr kind)))
             (lambda (sr k)
               (m sr (lambda (v)
                       (k (global-update! i v)))))))
          ((predefined)
           (compiler-error "Assignment to immutable variable" n)))
        (compiler-error "No variable" n))))

;; Lambdas

;; arity+1, and size+1 above, because we may have to collect up extra
;; arguments into a list when we do the application.

(define (meaning-fix-abstraction n* e+ r)
  (let* ((arity (length n*))
         (arity+1 (+ 1 arity))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2)))
    (lambda (sr k)
      (k (lambda (v* k1)
           (if (= (vector-length (:args v*)) arity+1)
               (m+ (sr-extend* sr v*) k1)
               (runtime-error "Incorrect arity" arity)))))))

(define (meaning-dotted-abstraction n* n e+ r)
  (let* ((arity (length n*))
         (arity+1 (+ 1 arity))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2)))
    (lambda (sr k)
      (k (lambda (v* k1)
           (if (>= (vector-length (:args v*)) arity+1)
               (begin (listify! v* arity)
                      (m+ (sr-extend* sr v*) k1))
               (runtime-error "Insufficient args" v*)))))))

;; Takes rest args, conses them into a list, and pops them into the
;; magical extra activation frame slot. Interesting point from Tony:
;; when `apply`ing a procedure, you don't want to be taking the list
;; or arguments apart just to put it back together, so it's worth
;; having a different entry point for `apply`. Extra for experts ..
(define (listify! v* arity)
  (let loop ((index (- (vector-length (:args v*)) 1))
             (result '()))
    (if (= arity index)
        (:argument! v* arity result)
        (loop (- index 1)
              (cons (:argument v* (- index 1)) result)))))

(define (meaning-abstraction nn* e+ r)
  (let parse ((n* nn*)
              (regular '()))
    (cond
      ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
      ((null? n*) (meaning-fix-abstraction nn* e+ r))
      (else (meaning-dotted-abstraction (reverse regular) n* e+ r)))))

;; Now for the repl

(define r.init '())
(define sr.init (make <activation> 0))

(define g.current '())
(define g.init '())

(define (repl)
  (define (toplevel)
    (display "> ")
    ((meaning (read) r.init) sr.init display)(newline)
    (toplevel))
  (toplevel))
