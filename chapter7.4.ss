;; Follows on from the first chapter 7 compiler by compiling to byte
;; codes, which can be more readily serialised to files.

(load "prelude.ss")
(load "env.ss")
(load "closure.ss")


;; Registers
(define *env* undefined-value)
(define *val* undefined-value)
(define *stack* undefined-value)

(define (stack-new) '())

(define (stack-push v)
  (set! *stack* (cons v *stack*)))

(define (stack-pop)
  (let ((v (car *stack*)))
    (set! *stack* (cdr *stack*))
    v))

(define (stack-save)
  *stack*)

(define (stack-restore s)
  (set! *stack* s))

(define *fun* undefined-value)

(define *arg1* undefined-value)
(define *arg2* undefined-value)

(define *pc* undefined-value)

;; An extra register for literals in a program
(define *constants* (make-vector 100))

;; And one to hold our eventual escape procedure
(define *exit* 'blowup)

;; OK registers and stack done. Now, a means to define our opcodes.
;; Taken straight from the book.

(define (fetch-byte)
  (let ((byte (car *pc*)))
    (set! *pc* (cdr *pc*))
    byte))

(define-syntax define-instruction-set
  (syntax-rules (define-instruction)
    ((define-instruction-set
       step instruction-size instruction-decode
       (define-instruction (name . args) opcode . body)
       ...)
     (begin
       ;; I'd rather have a stepper here
       (define (step)
         (let ((instruction (fetch-byte)))
           (case instruction
             ((opcode) (run-clause args body))
             ...)))

       ;; In my compiler, the program counter is a cons cell, so it
       ;; represents both the program code and an instruction or
       ;; argument
       (define (instruction-size pc)
         (case (car pc)
           ((opcode) (size-of-clause args))
            ...))
       (define (instruction-decode pc)
         ;; Renamed, this is confusing in the book
         (define (local-fetch-byte)
           (let ((byte (car pc)))
             (set! pc (cdr pc))
             byte))
         
         (let-syntax
             ((decode-clause
               (syntax-rules ()
                 ((decode-clause instruction ()) '(instruction))
                 ((decode-clause instruction (a))
                  (let ((a (local-fetch-byte))) `(instruction ,a)))
                 ((decode-clause instruction (a b))
                  (let* ((a (local-fetch-byte))
                         (b (local-fetch-byte))) `(instruction ,a ,b))))))
           (let ((instruction (local-fetch-byte)))
             (case instruction
               ((opcode) (decode-clause name args))
               ...))))))))

(define-syntax run-clause
  (syntax-rules ()
    ((run-clause () body) (begin . body))
    ((run-clause (a) body)
     (let ((a (fetch-byte))) . body))
    ((run-clause (a b) body)
     (let* ((a (fetch-byte)) (b (fetch-byte)))
       . body))))

(define-syntax size-of-clause
  (syntax-rules ()
    ((size-clause ())    1)
    ((size-clause (a))   2)
    ((size-clause (a b)) 3)))

;; Phew! All done with the macros. Now we have to actually specify the
;; combinators and instructions!

;; verify that the proposed operand is indeed a byte
(define (check-byte i)
  (unless (and (>= i 0) (< i 256))
    (compiler-error "This number does not fit in a byte!" i)))

(define (SHALLOW-ARGUMENT-REF index) (list 5 index))
  
;; In the previous compiler I wasn't sure why this and
;; SET-SHALLOW-ARGUMENT! had been separated. Now the answer is clear:
;; this is a combinator, and SET-SHALLOW-ARGUMENT! is an instruction.
(define (SHALLOW-ASSIGNMENT! j m)
  (append m (SET-SHALLOW-ARGUMENT! j)))

(define (SET-SHALLOW-ARGUMENT! j) (list 25 j))

(define (DEEP-ARGUMENT-REF level index) (list 6 level index))

(define (DEEP-ASSIGNMENT! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)))

(define (SET-DEEP-ARGUMENT! i j) (list 26 i j))

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)))
(define (SET-GLOBAL! i) (list 27 i))

;; This changes significantly, since we can sprinkle opcodes around
;; freely.
(define (PREDEFINED i)
  (check-byte i)
  (case i
    ((0 1 2 3 4 5 6 7 8) (list (+ 10 i))) ;; i.e., our opcodes for hard-wired
                                   ;; predefines start at 10
    (else (list 19 i)))) ;; and the generic instruction is straight after

;; This is used during compilation, to collect literals. When running
;; code, the quotations are put in the register *constants*.
(define *quotations* '())

;; As with predefined variables, common quotations are given their own
;; opcodes; some are reused from the predefines
(define (CONSTANT v)
  (cond
    ((eq? v #t)    (list 10))
    ((eq? v #f)    (list 11))
    ((eq? v '())   (list 12))
    ;; some allegedly commonly-used integers get their own opcodes
    ((equal? v -1) (list 80))
    ((equal? v 0)  (list 81))
    ((equal? v 1)  (list 82))
    ((equal? v 2)  (list 83))
    ((equal? v 3)  (list 84))
    ;; other integers up to 255 get an opcode and an operand of the
    ;; number itself
    ((and (integer? v) (<= 0 v) (< v 256)) ;; book has 255 here?
     (list 79 v))
    (else (EXPLICIT-CONSTANT v))))

;; NB no attempt to dedup constants
(define (EXPLICIT-CONSTANT v)
  (set! *quotations* (append *quotations* (list v)))
  (list 9 (- (length *quotations*) 1)))

;; Conditional and unconditional jumps
(define (JUMP-FALSE offset)
  (cond
    ((< offset 256) (list 31 offset)) ;; book has < 255??
    ((< offset (* 256 256))
     (let ((lower  (modulo offset 256))
           (higher (quotient offset 256)))
       (list 29 lower higher))) ;; NB little-endian
    (else (compiler-error "Jump too large" offset))))

(define (GOTO offset)
  (cond
    ((< offset 256) (list 30 offset)) ;; book has < 255??
    ((< offset (* 256 256))
     (let ((lower  (modulo offset 256))
           (higher (quotient offset 256)))
       (list 28 lower higher))) ;; NB little-endian
    (else (compiler-error "Jump too large" offset))))

(define (ALTERNATIVE m1 m2 m3)
  ;; GOTO can be different lengths now, so we have to do it first
  (let ((mm2 (append m2 (GOTO (length m3)))))
    (append m1 ;; result of test clause to *val*
            (JUMP-FALSE (length mm2)) ;; jump over success k (and goto)
            mm2 ;; success k, including GOTO
            m3)))

;; A bunch of simple ops
(define (EXTEND-ENV) (list 32))
(define (UNLINK-ENV) (list 33))
(define (PUSH-VALUE) (list 34))
(define (POP-ARG1) (list 35))
(define (POP-2ARG) (list 36))
(define (PRESERVE-ENV) (list 37))
(define (RESTORE-ENV) (list 38))
(define (POP-FUNCTION) (list 39))
(define (CREATE-CLOSURE offset) (list 40 offset))
(define (RETURN) (list 43))
(define (PACK-FRAME! arity) (list 44 arity))
(define (FUNCTION-INVOKE) (list 45))
(define (FUNCTION-GOTO) (list 46))
(define (POP-FRAME! rank) (list 64 rank))
(define (POP-CONS-FRAME! arity) (list 47 arity))
(define (ALLOCATE-FRAME size) (list 55 (+ size 1)))
(define (ALLOCATE-DOTTED-FRAME arity) (list 56 (+ arity 1)))
(define (ARITY>=? arity+1) (list 78 arity+1))
(define (ARITY=? n) (list 75 (+ n 1)))


;; These are "just" combinators

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

(define (SEQUENCE m m+)
  (append m m+))

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+))
(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)))

;; The CALL* opcodes are used when the head of a function is a symbol
;; resolving to a primitive. An opcode is reserved for each primitive,
;; effectively hard-wiring its definition into the byte-code
;; intepreter. This, CALL puts the arguments onto the stack, and
;; INVOKE calls the respective opcode.  (NB this means all the
;; primitives mentioned here must be declared as such, below)
(define (CALL0 address)
  (INVOKE0 address))

;; In the book it's assumed that (case ..) will work with procedures;
;; however, in SISC, native procedures do not have a repr of their
;; name, so they are not eqv? to their respective symbol. So, I have
;; to use cond and eq?.

(define (INVOKE0 address)
  (cond
    ((eq? address read)    (list 89))
    ((eq? address newline) (list 88))
    (else (compiler-error "Unknown primitive" address))))

(define (CALL1 address m1)
  (append m1 (INVOKE1 address)))

(define (INVOKE1 address)
  (cond
    ((eq? address car)     (list 90))
    ((eq? address cdr)     (list 91))
    ((eq? address pair?)   (list 92))
    ((eq? address symbol?) (list 93))
    ;; omit some ...
    (else (compile-error "Unknown primitive" address))))

(define (CALL2 address m1 m2)
  (append m1 ;; m1 -> *val*
          (PUSH-VALUE) ;; m1 -> stack
          m2 ;; m2 -> *val*
          (POP-ARG1) ;; m1 -> *arg1*
          (INVOKE2 address)))

(define (INVOKE2 address)
  (cond
    ((eq? address cons) (list 100))
    ((eq? address eq?)  (list 101))
    ;; no setcar!, no set-cdr!
    ((eq? address +)    (list 104))
    ((eq? address -)    (list 105))
    ((eq? address =)    (list 106))
    ((eq? address <)    (list 107))
    ((eq? address >)    (list 108))
    ((eq? address *)    (list 109))
    ((eq? address <=)   (list 110))
    ((eq? address >=)   (list 111))
    ;; remainder? who uses remainder?
    (else (compiler-error "Unknown primitive" address))))

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE)
          m2 (PUSH-VALUE)
          m3
          (POP-2ARG)
          (INVOKE3 address)))

(define (INVOKE3 address)
  (compiler-error "No known primitives of 3 arguments"))

(define (FIX-CLOSURE m+ arity)
  (define the-function
    (append (ARITY=? arity)
            (EXTEND-ENV)
            m+
            (RETURN)))
  (let ((goto (GOTO (length the-function))))
    (append (CREATE-CLOSURE (length goto))
            goto
            the-function)))

(define (REGULAR-CALL m m*)
  (append m (PUSH-VALUE)
          m*
          (POP-FUNCTION)
          (PRESERVE-ENV) (FUNCTION-INVOKE) (RESTORE-ENV)))

(define (TR-REGULAR-CALL m m*)
  (append m (PUSH-VALUE)
          m* (POP-FUNCTION)
          (FUNCTION-GOTO)))

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE) m* (POP-FRAME! rank)))

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE) m* (POP-CONS-FRAME! arity)))

(define (FINISH) (list 20))

;; Define closures, and an invoke that is told whether it's a tail
;; call or not
(define-method (initialize (<closure> self)
                           (<list> code)
                           (<activation> closed))
  (:code! self code)
  (:closed-env! self closed))

(define-method (invoke (<closure> f) (<boolean> tail?))
  (unless tail? (stack-push *pc*))
  (set! *env* (:closed-env f))
  (set! *pc* (:code f)))

;; Now our instructions

(define-instruction-set step instruction-size instruction-decode

  ;; Nothing in 0, it's bad luck!
  
  ;; The book expands this and set-shallow-argument into 0, 1, 2, 3
  ;; variations. I'm happy with just these for the minute.
  (define-instruction (SHALLOW-ARGUMENT-REF index)    5
    (set! *val* (:argument *env* index)))

  (define-instruction (DEEP-ARGUMENT-REF level index) 6
    (set! *val* (deep-fetch *env* level index)))

  (define-instruction (GLOBAL-REF index)              7
    (set! *val* (global-fetch index)))
  
  (define-instruction (CHECKED-GLOBAL-REF index)      8
    (let ((v (global-fetch index)))
      (if (eq? v undefined-value)
          (runtime-error "Uninitialised variable")
          ;; %% patch GLOBAL-REF into *pc*
          (set! *val* v))))
  
  (define-instruction (CONSTANT i)                    9
    (set! *val* (vector-ref *constants* i)))
  
  (define-instruction (PREDEFINED_HASHT)              10
    (set! *val* #t))
  (define-instruction (PREDEFINED_HASHF)              11
    (set! *val* #f))
  (define-instruction (PREDEFINED_NIL)                12
    (set! *val* '()))
  (define-instruction (PREDEFINED_CONS)               13
    (set! *val* (predef-fetch 3)))
  (define-instruction (PREDEFINED_CAR)                14
    (set! *val* (predef-fetch 4)))
  (define-instruction (PREDEFINED_CDR)                15
    (set! *val* (predef-fetch 5)))
  (define-instruction (PREDEFINED_PAIR?)              16
    (set! *val* (predef-fetch 6)))
  (define-instruction (PREDEFINED_SYMBOL?)            17
    (set! *val* (predef-fetch 7)))
  (define-instruction (PREDEFINED_EQ?)                18
    (set! *val* (predef-fetch 8)))

  (define-instruction (PREDEFINED j)                  19
    (set! *val* (predef-fetch j)))

  ;; Call the elsehwre-defined exit handler
  (define-instruction (FINISH)                        20
    (*exit* *val*))
  
  ;; %% SET-SHALLOW-ARGUMENT!0 through 3 start at 21
  
  (define-instruction (SET-SHALLOW-ARGUMENT! j)       25
    (:argument! *env* j *val*))

  (define-instruction (SET-DEEP-ARGUMENT! i j)        26
    (deep-update! *env* i j *val*))

  (define-instruction (SET-GLOBAL! i)                 27
    (global-update! i *val*))

  ;; ... and this is why the stack is a vector ..
  (define-instruction (LONG-GOTO lower higher)        28
    (let ((offset (+ (* higher 256) lower)))
      (set! *pc* (list-tail *pc* offset))))
  (define-instruction (LONG-JUMP lower higher)        29
    (if (not *val*)
        (let ((offset (+ (* higher 256) lower)))
          (set! *pc* (list-tail *pc* offset)))))

  (define-instruction (SHORT-GOTO offset)             30
    (set! *pc* (list-tail *pc* offset)))
  (define-instruction (SHORT-JUMP-FALSE offset)       31
    (if (not *val*)
        (set! *pc* (list-tail *pc* offset))))

  (define-instruction (EXTEND-ENV)                    32
    (set! *env* (extend *env* *val*)))
  (define-instruction (UNLINK-ENV)                    33
    (set! *env* (:next *env*)))

  (define-instruction (PUSH-VALUE)                    34
    (stack-push *val*))
  (define-instruction (POP-ARG1)                      35
    (set! *arg1* (stack-pop)))
  ;; NB here I differ from the book, this pops both arg registers, in
  ;; the same opcode
  (define-instruction (POP-2ARG)                      36
    (begin (set! *arg2* (stack-pop))
           (set! *arg1* (stack-pop))))
  (define-instruction (PRESERVE-ENV)                  37
    (stack-push *env*))
  (define-instruction (RESTORE-ENV)                   38
    (set! *env* (stack-pop)))
  (define-instruction (POP-FUNCTION)                  39
    (set! *fun* (stack-pop)))

  (define-instruction (CREATE-CLOSURE offset)         40
    (set! *val*
      (make <closure>
            (list-tail *pc* offset) *env*)))

  ;; 41, 42 not present in the book (code). Maybe later?
  
  (define-instruction (RETURN)                        43
    (set! *pc* (stack-pop)))

  (define-instruction (PACK-FRAME! arity)             44
    (listify! *val* arity))
  
  (define-instruction (FUNCTION-INVOKE)               45
    (invoke *fun* #f))
  (define-instruction (FUNCTION-GOTO)                 46
    (invoke *fun* #t))

  (define-instruction (POP-CONS-FRAME! arity)         47
    (:argument! *val* arity
                (cons (stack-pop)
                      (:argument *val* arity))))

  ;; 48, 49 free
  
  ;; %% ALLOCATE-FRAME of sizes 1-5 start at 50
  (define-instruction (ALLOCATE-FRAME size+1)         55
    (set! *val* (make <activation> size+1)))

  (define-instruction (ALLOCATE-DOTTED-FRAME arity+1) 56
    (let ((v* (make <activation> arity+1)))
      (:argument! v* (- arity+1 1) '())
      (set! *val* v*)))

  ;; 57, 58, 59 free
  
  ;; %% 60-63 specialised versions of pop-frame
  (define-instruction (POP-FRAME! rank)               64
    (:argument! *val* rank (stack-pop)))

  ;; 65-69 free
  
  ;; %% 70-74 ARITY=? from 0-4

  (define-instruction (ARITY=? arity+1)               75
    (unless (= (:length *val*) arity+1)
      (runtime-error "Expected " (- arity+1 1)
                     "arguments; got "
                     (- (:length *val*) 1))))

  ;; 76, 77 free
  
  ;; No specialisations, since these are used for dotted lambdas.
  (define-instruction (ARITY>=? arity+1)              78
    (if (< (:length *val*) arity+1)
        (runtime-error "Too few arguments")))
  
  ;; integer 0 <= i < 256
  (define-instruction (INT i)                         79
    (set! *val* i))
  
  (define-instruction (INT_NEG1)                      80
    (set! *val* -1))
  (define-instruction (INT_0)                         81
    (set! *val* 0))
  (define-instruction (INT_1)                         82
    (set! *val* 1))
  (define-instruction (INT_2)                         83
    (set! *val* 2))
  (define-instruction (INT_3)                         84
    (set! *val* 3))

  ;; inlined primitives
  (define-instruction (CALL0-newline)                 88
    (newline))
  (define-instruction (CALL0-read)                    89
    (set! *val* (read)))

  (define-instruction (CALL1-car)                     90
    (set! *val* (car *val*)))
  (define-instruction (CALL1-cdr)                     91
    (set! *val* (cdr *val*)))
  (define-instruction (CALL1-pair?)                   92
    (set! *val* (car *val*)))
  (define-instruction (CALL1-symbol?)                 93
    (set! *val* (car *val*)))
  ;; omitted: display

  (define-instruction (CALL2-cons)                    100
    (set! *val* (cons *arg1* *val*)))
  (define-instruction (CALL2-eq?)                     101
    (set! *val* (eq? *arg1* *val*)))
  (define-instruction (CALL2-PLUS)                    104
    (set! *val* (+ *arg1* *val*)))
  (define-instruction (CALL2-MINUS)                   105
    (set! *val* (- *arg1* *val*)))
  (define-instruction (CALL2-EQUAL)                   106
    (set! *val* (= *arg1* *val*)))
  (define-instruction (CALL2-LT)                      107
    (set! *val* (< *arg1* *val*)))
  (define-instruction (CALL2-GT)                      108
    (set! *val* (> *arg1* *val*)))
  (define-instruction (CALL2-TIMES)                   109
    (set! *val* (* *arg1* *val*)))
  (define-instruction (CALL2-LTE)                     110
    (set! *val* (<= *arg1* *val*)))
  (define-instruction (CALL2-GTE)                     111
    (set! *val* (>= *arg1* *val*)))
  
  ) ;; end of define-instruction-set

(load "pretreat.ss")

;; Execution

(define (compile/standalone expression)
  (set! *quotations* '())
  (let* ((m (meaning expression r.init #t))
         (finish-pc (code-epilogue))
         (start-pc (append m (RETURN)))
         (global-names (map car (reverse g.current)))
         (constants (apply vector *quotations*)))
    (lambda ()
      (run-machine start-pc finish-pc constants global-names))))

(define (disassemble pc)
  (let loop ((pc pc)
             (instructions '()))
    (cond
      ((null? pc) (reverse instructions))
      (else (loop (list-tail pc (instruction-size pc))
                  (cons (instruction-decode pc) instructions))))))

(define *debug* #f)

(define (run)
  (step) (run))

(define (display-vm-state)
  (display "*val* ")(display (show *val*))(newline)
  (display "*fun* ")(display (show *fun*))(newline)
  (display "=== *stack* ===")(newline)
  (display-stack *stack*)
  (display "===============")(newline))

(define (display-stack sp)
  (for-each (lambda (value) (display (show value))(newline))
            sp))

(define-generics show)
(define-method (show (<activation> frame))
  (let ((out (open-output-string))) ;; SISC-specific
    (with-output-to-port out
      (lambda ()
        (display "<frame")
        (do ((i 0 (+ i 1)))
            ((= i (:length frame)))
          (display " ")
          (display (show (:argument frame i))))
        (display ">")))
    (get-output-string out)))

(define-method (show (<value> val))
  val)

(define (run-debug)
  (display "=== vm state ===")(newline)
  (display-vm-state)
  (display "*pc* ")
  (display (instruction-decode *pc*))(newline)
  (step)(newline)
  (run-debug))

;; I do this as an epilogue rather than a prologue, so that the jump
;; is "forward". But it doesn't really matter, since it's floating in
;; spa-ha-hace ..
(define (code-epilogue)
  (FINISH))

(define (run-machine start finish constants global-names)
  (set! *constants* constants)
  (set! sg.current
    (make-vector (length global-names) undefined-value))
  ;; Not bothering with globals symbol table for the minute
  (set! *env* sr.init)
  (set! *stack* (stack-new))
  (set! *val* undefined-value)
  (set! *fun* undefined-value)
  (set! *arg1* undefined-value)
  (set! *arg2* undefined-value)
  (stack-push finish)
  (set! *pc* start)
  ;; No code vector; the pc encodes it
  (call/cc (lambda (k)
             (set! *exit* k)
             (if *debug* (run-debug) (run)))))

(define (compile+run expression)
  ((compile/standalone expression)))

(define (repl)
  (display "> ")
  (let ((in (read)))
    (display (compile+run in))(newline)
    (repl)))

;; For smoketest
(define eval-expr compile+run)

;; The book uses a class for primitives, because they don't have a
;; closed-over environment

(define-generics :address :address!)

(define-class (<primitive>)
  (address :address :address!))

(define-method (initialize (<primitive> self)
                           (<procedure> address))
  (:address! self address))

(define-method (invoke (<primitive> f) (<boolean> tail?))
  (unless tail? (stack-push *pc*))
  ((:address f)))

(define (define-primitive name underlying arity)
  (let ((arity+1 (+ arity 1)))
    (case arity
      ((0)
       (define-initial name
         (let ((behaviour
                (lambda ()
                  (if (= (:length *val*) arity+1)
                      (begin
                        (set! *val* (underlying))
                        (set! *pc* (stack-pop)))
                      (runtime-error
                       "Incorrect arity for" name
                       "need:" arity
                       "got: " (- (:length *val*) 1))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <primitive> behaviour))))
      ((1)
       (define-initial name
         (let ((behaviour
                (lambda ()
                  (if (= (:length *val*) arity+1)
                      (begin
                        (set! *val* (underlying (:argument *val* 0)))
                        (set! *pc* (stack-pop)))
                      (runtime-error
                       "Incorrect arity for" name
                       "need:" arity
                       "got: " (- (:length *val*) 1))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <primitive> behaviour))))
      ((2)
       (define-initial name
         (let ((behaviour
                (lambda ()
                  (if (= (:length *val*) arity+1)
                      (begin
                        (set! *val*
                          (underlying (:argument *val* 0)
                                      (:argument *val* 1)))
                        (set! *pc* (stack-pop)))
                      (runtime-error
                       "Incorrect arity for" name
                       "need:" arity
                       "got: " (- (:length *val*) 1))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <primitive> behaviour))))
      ((3)
       (define-initial name
         (let ((behaviour
                (lambda ()
                  (if (= (:length *val*) arity+1)
                      (set! *val*
                        (underlying (:argument *val* 0)
                                    (:argument *val* 1)
                                    (:argument *val* 2)))
                      (runtime-error
                       "Incorrect arity for" name
                       "need:" arity
                       "got: " (- (:length *val*) 1))))))
           (description-extend!
            name `(function ,underlying ,arity))
           (make <primitive> behaviour)))))))

;; We have to be careful that these match the hard-wired predefines
(define-initial 't #t)
(define-initial 'f #f)
(define-initial 'nil '())
(define-primitive 'cons cons 2)
(define-primitive 'car car 1)
(define-primitive 'cdr cdr 1)
(define-primitive 'pair? pair? 1)
(define-primitive 'symbol? symbol? 1)
(define-primitive 'eq? eq? 2)

;; From here on they can be in any order
(define-primitive 'read read 0)
(define-primitive 'newline newline 0)

(define-primitive 'car car 1)
(define-primitive 'cdr cdr 1)
(define-primitive 'pair? pair? 1)
(define-primitive 'symbol? symbol? 1)

(define-primitive '+ + 2)
(define-primitive '- - 2)
(define-primitive '= = 2)
(define-primitive '< < 2)
(define-primitive '> > 2)
(define-primitive '<= <= 2)
(define-primitive '>= >= 2)
(define-primitive '* * 2)

(define-initial 'apply
  (let ((arity 2) (arity+1 3))
    (make <primitive>
          (lambda ()
            (if (>= (:length *val*) arity+1)
                (let* ((f (:argument *val* 0)) ;; the function
                       (last-arg-index (- (:length *val*) 2))
                       (last-arg (:argument *val* last-arg-index))
                       (size (+ last-arg-index (length last-arg)))
                       (frame (make <activation> size)))
                  (do ((i 1 (+ i 1)))
                      ((= i last-arg-index))
                    (:argument! frame (- i 1) (:argument v* i)))
                  (do ((i (- last-arg-index 1) (+ i 1))
                       (last-arg last-arg (cdr last-arg)))
                      ((null? last-arg))
                    (:argument! frame i (car last-arg)))
                  (set! *val* frame)
                  (set! *fun* f) ;; not strictly necessary, but
                  ;; useful if we e.g., examine the
                  ;; registers while debugging (trick
                  ;; taken from the book code)
                  (invoke f #t))
                (runtime-error
                 "Incorrect arity for" name
                 "need at least:" arity
                 "got: " (- (:length *val*) 1)))))))

(define-initial 'list
  (make <primitive>
        (lambda ()
          (let loop ((index (- (:length *val*) 2))
                     (result '()))
            (cond ((= index -1)
                   (set! *val* result)
                   (set! *pc* (stack-pop)))
                  (else (loop (- index 1)
                              (cons (:argument *val* index) result))))))))

;; I didn't bother for the previous compiler, but for this one I'll
;; make a class for continuations

(define-generics :stack :stack!)

(define-class (<continuation>)
  (stack :stack :stack!))

(define-method (initialize (<continuation> self)
                           (<list> stack))
  (:stack! self stack))

(define-method (invoke (<continuation> self)
                       (<boolean> tail?))
  (if (= (:length *val*) 2)
      (begin
        (set! *val* (:argument *val* 0))
        (stack-restore (:stack self))
        (set! *pc* (stack-pop)))
      (runtime-error
       "Expected one argument to continuation;"
       "got " (- (:length *val*) 1))))

(define-method (show (<continuation> _self))
  "Continuation (mysterious and otherwordly)")


(define-initial 'call/cc
  (make <primitive>
        (lambda ()
          (if (= (:length *val*) 2)
              (let* ((f (:argument *val* 0))
                     (frame (make <activation> 2)))
                (:argument! frame 0 (make <continuation> (stack-save)))
                (set! *val* frame)
                (set! *fun* f) ;; debug purposes
                (invoke f #t))
              (runtime-error "Expected one argument, got "
                             (- (:length *val*) 1))))))
