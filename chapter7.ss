;; This is a compiler, adapted from the "pretreating" interpreter of
;; the previous chapter. It takes the conversion to combinators a step
;; further by serialising them to a byte code, introducing a few
;; conveniences along the way, and writing a byte code interpreter.

(load "prelude.ss")
(load "env.ss")
(load "closure.ss")

;; Byte code interpreter

;; The environment register, as in the previous chapter.
(define *env* sr.init)

;; The *val* register is used to store values for subsequent
;; instructions to operate on
(define *val* undefined-value)

;; We need a stack for ad-hoc registers. The book uses a vector and a
;; stack pointer. I'm going to use a list, just to be different.
(define *stack* '())

(define (stack-push v)
  (set! *stack* (cons v *stack*)))

(define (stack-pop)
  (let ((v (car *stack*)))
    (set! *stack* (cdr *stack*))
    v))

;; When calling a function, we need to store the head as well as the
;; arguments, so here's another register for that.
(define *fun* undefined-value)

;; Since we support primitives with up to three arguments, we need
;; registers to hold the first and second of those (the third can come
;; straight from *val*).
(define *arg1* undefined-value)
(define *arg2* undefined-value)

;; Finally, a register for the next instruction to execute.
(define *pc* undefined-value)

;; OK, now there are sufficient registers (and a stack) so we can
;; rewrite the combinators purely as operations on registers (and the
;; stack). This means we can linearise programs, since they no longer
;; need an implicit stack and variables. In this chapter, a program is
;; a list of thunks; the program counter *pc* is a cons cell.

;; The examples given in §7.1.1 and §7.1.2 don't fully linearise the
;; instructions -- there's still a bit of recursion in evaluating
;; sequences or arguments. I'll take up at §7.1.3, where the
;; combinators start to be defined as lists of thunks.

(define (CONSTANT v)
  (list (lambda () (set! *val* v))))

;; Not sure why this is now split into two similarly named
;; combinators, but maybe that will become clear.
(define (SHALLOW-ASSIGNMENT! j m)
  (append m (SET-SHALLOW-ARGUMENT! j)))
(define (SET-SHALLOW-ARGUMENT! j)
  (list (lambda () (:argument! *env* j *val*))))

(define (SHALLOW-ARGUMENT-REF index)
  (list (lambda () (set! *val* (:argument *env* index)))))

(define (DEEP-ARGUMENT-REF level index)
  (list (lambda () (set! *val* (deep-fetch *env* level index)))))

(define (CHECKED-GLOBAL-REF index)
  (list (lambda ()
          (let ((v (global-fetch index)))
            (if (eq? v undefined-value)
                (runtime-error "Uninitialised variable")
                (set! *val* v))))))

(define (PREDEFINED i)
  (list (lambda () (set! *val* (predef-fetch i)))))

;; Conditional and unconditional jumps
(define (JUMP-FALSE i)
  (list (lambda ()
          (if (not *val*)
              (set! *pc* (list-tail *pc* i))))))

(define (GOTO i)
  (list (lambda ()
          (set! *pc* (list-tail *pc* i)))))

(define (ALTERNATIVE m1 m2 m3)
  (append m1 ;; result of test clause to *val*
          (JUMP-FALSE (+ 1 (length m2))) ;; jump over success k (and goto)
          m2 ;; success k
          (GOTO (length m3)) ;; then jump past failure k
          m3))

;; Closures aren't invoked with an activation frame as in the last
;; chapter; the arguments are in the *val* register. The 'code' of a
;; closure is a program counter.

(define-method (initialize (<closure> self)
                           (<list> code)
                           (<activation> closed))
  (:code! self code)
  (:closed-env! self closed))

(define-method (invoke (<closure> f))
  (stack-push *pc*)
  (set! *env* (:closed-env f))
  (set! *pc* (:code f)))

(define (REGULAR-CALL m m*)
  (append m (PUSH-VALUE) ;; put *val* onto the stack
          m* ;; collect the arguments, stack should end up where it is
             ;; now
          (POP-FUNCTION) ;; put the top of the stack in *fun*
          (PRESERVE-ENV)
          (FUNCTION-INVOKE)
          (RESTORE-ENV)))

(define (PUSH-VALUE)
  (list (lambda () (stack-push *val*))))
(define (POP-FUNCTION)
  (list (lambda () (set! *fun* (stack-pop)))))

;; store the env on the stack
(define (PRESERVE-ENV)
  (list (lambda () (stack-push *env*))))
(define (RESTORE-ENV)
  (list (lambda () (set! *env* (stack-pop)))))

(define (FUNCTION-INVOKE)
  (list (lambda () (invoke *fun*))))

(define (NARY-CLOSURE m+ arity)
  (define the-function
    (append (ARITY>=? (+ arity 1)) ;; bail if not enough arguments
            (PACK-FRAME! arity) ;; collect varargs
            (EXTEND-ENV) ;; extend *env* with arguments
            m+ ;; execute the forms
            (RETURN)))
  (append (CREATE-CLOSURE 1) ;; make a closure that starts at pc+1
          (GOTO (length the-function)) ;; skip the definition
          the-function))

(define (CREATE-CLOSURE offset)
  (list (lambda ()
          (set! *val*
            (make <closure> (list-tail *pc* offset) *env*)))))

;; Get all the arguments after arity+1 and put them in a list, in the
;; arity+1th slot. This is for when we're calling a dotted
;; abstraction. Thought: at the expense of complicating closure code,
;; the closure could be the one to pop arguments as required (but it
;; would need to know how many are there I guess)
(define (listify! v* arity)
  (let loop ((index (- (vector-length (:args v*)) 1))
             (result '()))
    (if (= arity index)
        (:argument! v* arity result)
        (loop (- index 1)
              (cons (:argument v* (- index 1)) result)))))

(define (PACK-FRAME! arity)
  (list (lambda () (listify! *val* arity))))

(define (RETURN)
  (list (lambda () (set! *pc* (stack-pop)))))

(define (DEEP-ASSIGNMENT! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)))
(define (SET-DEEP-ARGUMENT! i j)
  (list (lambda () (deep-update! *env* i j *val*))))

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)))
(define (SET-GLOBAL! i)
  (list (lambda () (global-update! i *val*))))

(define (SEQUENCE m m+)
  (append m m+))

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+))
(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)))

(define (EXTEND-ENV)
  (list (lambda () (set! *env* (extend *env* *val*)))))

;; If we're not tail-calling, we need to restore the environment;
;; luckily, it'll always be the next pointer of the current
;; environment
(define (UNLINK-ENV)
  (list (lambda () (set! *env* (:next *env*)))))

(define (CALL0 address)
  (list (lambda () (INVOKE0 address))))

(define (INVOKE0 address)
  (list (lambda ()
          (set! *val* (address)))))

(define (CALL1 address m1)
  (append m1 (INVOKE1 address)))

(define (INVOKE1 address)
  (list (lambda ()
          (set! *val* (address *val*)))))

(define (CALL2 address m1 m2)
  (append m1 ;; m1 -> *val*
          (PUSH-VALUE) ;; *val* -> stack
          m2 ;; m2 -> *val*
          (POP-ARG1) ;; stack -> *arg1*
          (INVOKE2 address)))

(define (INVOKE2 address)
  (list (lambda () (set! *val* (address *arg1* *val*)))))

(define (PUSH-VALUE)
  (list (lambda () (stack-push *val*))))

(define (POP-ARG1)
  (list (lambda () (set! *arg1* (stack-pop)))))

;; We only ever pop just arg1 or both arg1 and arg2, so instead of pop-arg2 I have pop-2arg
(define (POP-2ARG)
  (list (lambda ()
          (set! *arg2* (stack-pop))
          (set! *arg1* (stack-pop)))))

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE)
          m2 (PUSH-VALUE)
          m3
          (POP-2ARG)
          (INVOKE3 address)))

(define (INVOKE3 address)
  (list (lambda () (set! *val* (address *arg1* *arg2* *val*)))))

(define (FIX-CLOSURE m+ arity)
  (define the-function
    (append (ARITY=? arity)
            (EXTEND-ENV)
            m+
            (RETURN)))
  (append (CREATE-CLOSURE 1)
          (GOTO (length the-function))
          the-function))

(define (REGULAR-CALL m m*)
  (append m (PUSH-VALUE)
          m* (POP-FUNCTION)
          (PRESERVE-ENV) (FUNCTION-INVOKE) (RESTORE-ENV)))

(define (TR-REGULAR-CALL m m*)
  (append m (PUSH-VALUE)
          m* (POP-FUNCTION)
          (FUNCTION-INVOKE)))

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE) m* (POP-FRAME! rank)))

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE) m* (POP-CONS-FRAME! arity)))

;; put the top-most value in the stack into the given frame slot;
;; used to build activation frames of arguments. Calls end up being:
;; push argument ...
;; make frame
;; set popped argument ...
(define (POP-FRAME! rank)
  (list (lambda ()
          (:argument! *val* rank (stack-pop)))))

;; cons the top-most value 
(define (POP-CONS-FRAME! arity)
  (list (lambda ()
          (:argument! *val* arity (cons (stack-pop)
                                        (:argument *val* arity))))))

(define (ALLOCATE-FRAME size)
  (let ((arity+1 (+ size 1)))
    (list (lambda ()
            (set! *val* (make <activation> arity+1))))))

(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (list (lambda ()
            (set! *val* (let ((v* (make <activation arity+1)))
                          (:argument! v* arity '())
                          v*))))))

;; Check the number of args and bail if no good
(define (ARITY>=? n)
  (let ((arity+1 (+ n 1)))
    (list (lambda () (if (< (:length *val*) arity+1)
                         (runtime-error "Too few arguments"))))))

(define (ARITY=? n)
  (let ((arity+1 (+ n 1)))
    (list (lambda () (if (not (= (:length *val*) arity+1))
                         (runtime-error "Wrong number of arguments"
                                        n (- (:length *val*) 1)))))))


;; That's the combinators, now linearising to a list of thunks
;; operating on registers and a stack. The pretreatment remains the
;; same:
(load "pretreat.ss")

;; Here's our executor
(define (step)
  (let ((instruction (car *pc*)))
    (set! *pc* (cdr *pc*))
    (instruction)))

(define (run)
  (cond ((null? *pc*) *val*)
        (else (begin (step) (run)))))

(define (compile expression)
  (meaning expression sr.init #t))

(define (compile+run expression)
  (set! *pc* (compile expression))
  (run))

(define (repl)
  (display "> ")
  (let ((in (read)))
    (display (compile+run in))(newline)
    (repl)))

;; For smoketest
(define eval-expr compile+run)

(define (define-primitive name underlying arity)
  (let ((arity+1 (+ arity 1)))
    (case arity
      ((0)
       (define-initial name
         (let ((behaviour
                (append
                 (ARITY=? arity)
                 (INVOKE0 underlying))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <closure> behaviour sr.init))))
      ((1)
       (define-initial name
         (let ((behaviour
                (append
                 (ARITY=? arity)
                 (list (lambda ()
                         (set! *val*
                           (underlying (:argument *val* 0))))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <closure> behaviour sr.init))))
      ((2)
       (define-initial name
         (let ((behaviour
                (append
                 (ARITY=? arity)
                 (list (lambda ()
                         (set! *val*
                           (underlying (:argument *val* 0)
                                       (:argument *val* 1))))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <closure> behaviour sr.init))))
      ((3)
       (define-initial name
         (let ((behaviour
                (append
                 (ARITY=? arity)
                 (list (lambda ()
                         (set! *val*
                           (underlying (:argument *val* 0)
                                       (:argument *val* 1)
                                       (:argument *val* 2))))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <closure> behaviour sr.init)))))))

(define-primitive '+ + 2)
