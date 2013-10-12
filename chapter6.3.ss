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

(load "prelude.ss")
(load "env.ss")
(load "closure.ss")

;; Closures are a lambda and the closed-over environment
(define-method (initialize (<closure> self)
                           (<procedure> code)
                           (<activation> closed))
  (:code! self code)
  (:closed-env! self closed))

;; closures are invoked with an activation frame of the arguments
(define-method (invoke (<closure> f)
                       (<activation> v*))
  ((:code f) v* (:closed-env f)))

;; Frame pointer
(define *env* undefined-value)

;; Right then. Combinators.

(define (CONSTANT v)
  (lambda () v))

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

(define (ALTERNATIVE mc mt mf)
  (lambda () (if (mc) (mt) (mf))))

(define (SHALLOW-ASSIGNMENT index m)
  (lambda () (:argument! *env* index (m))))

(define (DEEP-ASSIGNMENT level index m)
  (lambda () (deep-update! *env* level index (m))))

(define (GLOBAL-SET! index m)
  (lambda () (global-update! index (m))))

(define (SEQUENCE m m+)
  (lambda () (m) (m+)))

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
        (:argument! v* arity '())
        v*))))

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


;; Now the pretreater, which is common to chapter 6 and 7 I think
(load "pretreat.ss")

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

;; For the smoketest
(define (eval-expr e)
  (set! *env* sr.init)
  ((meaning e r.init #t)))

(define-primitive '+ + 2)
(define-primitive '- - 2)

;; Exercise 6.2
;;
;; NB that NARY-CLOSURE uses the *env* register, so we need to give
;; that a value before we can invoke the constructed value.
(set! *env* sr.init)
(define-initial 'list ((NARY-CLOSURE (SHALLOW-ARGUMENT-REF 0) 0)))

;; Exercise 6.4 might be interesting to come back to ..
