;; This interpreter retreats from using generic procedures and
;; classes, instead using closures and message passing to represent
;; pretty much everything. The heap is explicit: every value is
;; allocated (given a location) and the environment maps variable
;; names to locations. (Obviously this is in preparation for
;; allocation *really* being explicit). Procedures and continuations
;; are also closures, but not invoked directly. The interpreter still
;; implicitly CPS-transforms expressions.

;; By now there's little confusion between meta- and object-level, so
;; I'm giving up on the $ prefix.

;; Standard defn.
(define (wrong . args)
  (error args))

;; Memory: this is threaded through the interpreter control flow, as
;; an argument to each (interpreting) procedure.

;; Memory and envs will use the same implementation: a procedure that
;; looks up the address or name given.

(define (update s a v)
  (lambda (aa)
    (if (eqv? a aa)
        v
        (s aa))))

;; Convenience for multiple assignments
(define (update* s a* v*)
  (if (pair? a*)
      (update* (update s (car a*) (car v*))
               (cdr a*) (cdr v*))
      s))

;; Interface for other procedures.
;; args: number of addresses to allocate, current memory, (meta-level)
;; continuation to call with the new memory and list of allocated addresses.
(define (allocate n s q)
  (if (> n 0)
      (let ((a (new-location s)))
        (allocate (- n 1)
                  (expand-store a s)
                  (lambda (a* ss)
                    (q (cons a a*) ss))))
      (q '() s)))

;; Reserve an address. This just uses the first memory location as a
;; high water mark; as such, it's coupled with new-location and init
(define (expand-store high-location s)
  (update s 0 high-location))

(define (new-location s)
  (+ 1 (s 0)))

(define (initial-s)
  (expand-store 0 (lambda _ ($wrong "No such address"))))

;; Values. This is the first 'message passing' style abstraction.
;; There's two messages all values respond to: 'type and 'boolify.

;; boolify returns one of these
(define TRUE (lambda (x y) x))
(define FALSE (lambda (x y) y))

;; '() is a singleton
(define NULL
  (lambda (msg)
    (case msg
      ((type) 'null)
      ((boolify) TRUE))))

(define (create-boolean v)
  (let ((bool (if v TRUE FALSE)))
    (lambda (msg)
      (case msg
        ((type) 'boolean)
        ((boolify) bool)))))

;; symbols can be asked for their name
(define (create-symbol s)
  (lambda (msg)
    (case msg
      ((type) 'symbol)
      ((boolify) TRUE)
      ((name) s))))

(define (create-number n)
  (lambda (msg)
    (case msg
      ((type) 'number)
      ((boolify) TRUE)
      ((value) n))))

(define (create-string s)
  (lambda (msg)
    (case msg
      ((type) 'string)
      ((boolify) TRUE)
      ((value) s))))

(define (create-function tag behaviour)
  (lambda (msg)
    (case msg
      ((type) 'function)
      ((boolify) TRUE)
      ((tag) tag)
      ((behaviour) behaviour))))


;; These may be mutated; thus set-car and set-cdr are functions that
;; transform a memory.
(define (create-pair a d)
  (lambda (msg)
    (case msg
      ((type) 'pair)
      ((boolify) TRUE)
      ((set-car) (lambda (s v) (update s a v)))
      ((set-cdr) (lambda (s v) (update s d v)))
      ((car) a)
      ((cdr) d))))

;; As with $allocate above, must be given a continuation. In this case
;; it will be given the pair value and the new memory.
(define (allocate-pair a d s q)
  (allocate 2 s
            (lambda (ad ss)
              (q (create-pair (car ad) (cadr ad))
                 (update* ss ad (list a d))))))

;; In this case the supplied continuation gets the head of the list
;; and the new memory.
(define (allocate-list v* s q)
  ;; This creates a chain of continuations that will allocate the
  ;; pairs; this starts unwinding when the condition fails, where
  ;; starts with the initial memory state.
  (define (consify vs qq)
    (if (pair? vs)
        (consify (cdr vs) (lambda (v ss)
                            (allocate-pair (car vs) v ss qq)))
        (qq NULL s)))
  (consify v* q))

;; Environment

;; update and update* already suffice

(define (initial-env)
  (lambda (n) (wrong "No binding for name" n)))

;; Evaluate

;; Now we take an extra arg for the memory. Note, however, that all of
;; the continuations dealt with in evaluate-* functions don't have an
;; argument for the env. That's because only function application can
;; introduce new names, and that recurses via evaluate. Mutation is
;; dealt with by 'updating' (constructing a new) memory, which *is* an
;; argument to continuations.
(define (evaluate expr env mem k)
  (if (pair? expr)
      (case (car expr)
        ((quote) (evaluate-quote (cadr expr) env mem k))
        ((if) (evaluate-if (cadr expr) (caddr expr) (cadddr expr) env mem k))
        ((begin) (evaluate-begin (cdr expr) env mem k))
        ((set!) (evaluate-set (cadr expr) (caddr expr) env mem k))
        ((lambda) (evaluate-lambda (cadr expr) (cddr expr) env mem k))
        (else (evaluate-apply (car expr) (cdr expr) env mem k))))

      (if (symbol? expr) (evaluate-var expr env mem k)
          (evaluate-quote expr env mem k)))

;; We now have to translate between our meta-level representations of
;; values and the object-level representations.
(define (evaluate-quote value env mem k)
  (meta->object value mem k))

(define (meta->object value mem q)
  (cond
    ((null? value) (q NULL mem))
    ((boolean? value) (q (create-boolean value) mem))
    ((symbol? value) (q (create-symbol value) mem))
    ((string? value) (q (create-string value) mem))
    ((number? value) (q (create-number value) mem))
    ((pair? value)
     (meta->object (car value) mem
                   (lambda (head mem1)
                     (meta->object (cdr value) mem1
                                   (lambda (tail mem2)
                                     (allocate-pair head tail mem2 q))))))))

;; For symmetry's sake, and e.g., to present evaluated values back again.

(define (object->meta value mem)
  (case (value 'type)
    ((null) '())
    ((boolean) ((value 'boolify) #t #f))
    ((symbol) ((value 'name)))
    ((string number) (value 'value))
    ((function) value) ;; hmm, better to return "opaque" rep?
    ((pair) (cons (object->meta (mem (value 'car)) mem)
                  (object->meta (mem (value 'cdr)) mem)))
    (else (wrong "Unknown object-level type" (value 'type)))))

;; If

(define (evaluate-if c t f env mem k)
  (evaluate c env mem
            (lambda (v mem1)
              (evaluate (if (v 'boolify) t f) env mem1 k))))

;; Begin

(define (evaluate-begin e* env mem k)
  (if (pair? (cdr e*))
      (evaluate (car e*) env mem
                (lambda (_ mem1)
                  (evaluate-begin (cdr e*) env mem1 k)))
      (evaluate (car e*) env mem k)))

;; variable ref

(define (evaluate-var name env mem k)
  (k (mem (env name)) mem))

;; set!

(define (evaluate-set name v-expr env mem k)
  (evaluate v-expr env mem
            (lambda (val mem1)
              (k val (update mem1 (env name) val)))))

;; lambda

;; When invoked, a function ('s behaviour) allocates space for each
;; argument, and updates the environment with the appropriate
;; bindings.

;; NB1 Following the book, this allocates space for the function and
;; uses that as its 'tag', but does not update memory to reflect
;; this. Presumably this anticipates further modification. (A: No,
;; it's so procedures can be compared)

;; Updated in exercise 4.7 to handle improper lists of arguments, and
;; an extension: handle a symbol meaning the entire list of args.

(define (evaluate-lambda names body env mem k)
  (let ((arity (arg-length names)))
    (allocate
     1 mem
     (lambda (a* mem')
       (k (create-function
           (car a*)
           (lambda (vals mem1 k)
             (allocate arity mem1
                       (lambda (as mem1')
                         (evaluate-begin body
                                         (update-env-args env names as)
                                         (update-store-args mem1' as vals names)
                                         k))))) mem')))))

;; Update the env according to the list of addresses. Since the
;; addresses have been allocated according to the arity of the
;; argument list, we can rely on it having the right number of
;; addresses.
(define (update-store-args s as vs names)
  (cond ((pair? names)
         (update-store-args (update s (car as) (car vs))
                            (cdr as) (cdr vs) (cdr names)))
        ((null? names) s)
        (else
          (allocate-list vs s (lambda (head ss)
                                (update ss (car as) head))))))

;; Again, trust the addresses to have the correct arity
(define (update-env-args env names as)
  (cond ((pair? names)
         (update-env-args (update env (car names) (car as))
                          (cdr names) (cdr as)))
        ((null? names) env)
        (else
          (update env names (car as)))))

;; this may not tell the whole story about whether an argument list is
;; the correcy arity
(define (arg-length names)
  (define (arg-length1 names n)
    (cond
      ((pair? names) (arg-length1 (cdr names) (+ 1 n)))
      ((null? names) n)
      (else (+ 1 n))))
  (cond
    ((pair? names) (arg-length1 (cdr names) 1))
    (else 1)))

;; (the book has it this way; technically it doesn't need to pass the
;; env to eval-args; maybe it's to allow modification in an exercise
;; later)
(define (evaluate-apply head args env mem k)
  (define (eval-args args env mem k)
    (if (pair? args)
        (evaluate (car args) env mem
                  (lambda (v mem1)
                    (eval-args (cdr args) env mem1
                               (lambda (vs mem2)
                                 (k (cons v vs) mem2)))))
        (k '() mem)))
  (evaluate head env mem
            (lambda (fn mem1)
              (if (eq? 'function (fn 'type))
                  (eval-args args env mem1
                             (lambda (allargs mem2)
                               ((fn 'behaviour) allargs mem2 k)))
                  (wrong "Attempted to apply a non-function" fn)))))


;; Toplevel

(define r.global (initial-env))
(define s.global (initial-s))
(define bottom-k (lambda (v) v))

;; The macros in the book have an egregious error: the second allocate
;; (inside the expansion of def-primitive) is basically a no-op, since
;; it's overwritten by the outer set!

(define (initial name value)
  (allocate 1 s.global
            (lambda (a* mem)
              (set! r.global (update r.global name (car a*)))
              (set! s.global (update mem (car a*) value)))))

(define (primitive name value)
  (initial name (create-function (new-location s.global) ;; I'm next here
                                 value)))

(initial 't (create-boolean #t))
(initial 'f (create-boolean #f))
(initial 'nil NULL)

(primitive '<=
           (lambda (args s k)
             (k (create-boolean
                 (<= ((car args) 'value) ((cadr args) 'value))) s)))
(primitive '*
           (lambda (args s k)
             (k (create-number
                 (apply * (map (lambda (v) (v 'value)) args)))
                s)))

(primitive 'eqv?
           (lambda (args s k)
             (k (create-boolean
                 (let ((a (car args)) (b (cadr args)))
                   (if (eq? (a 'type) (b 'type))
                       (case (a 'type)
                         ((null) #t)
                         ((boolean)
                          ((a 'boolify)
                           ((b 'boolify) #t #f)
                           ((b 'boolify) #f #t)))
                         ((symbol) (eq? (a 'name) (b 'name)))
                         ((string number)
                          (eq? (a 'value) (b 'value)))
                         ((pair)
                          (and (= (a 'car) (b 'car))
                               (= (a 'cdr) (b 'cdr))))
                         ((function)
                          (= (a 'tag) (b 'tab)))
                         (else #f))
                       #f))))))

;; Interesting! So, here, I get an object-level list of object-level
;; values as one of my arguments; to be able to apply the function I
;; have to have a meta-level list of object-level values, just like I
;; have in evaluate-apply.

(define (deref-list head s)
  (case (head 'type)
    ((null) '())
    ((pair)
     (cons (s (head 'car)) (deref-list (s (head 'cdr)) s)))))

(primitive 'apply
           (lambda (args s k)
             (let ((f (car args))
                   (head (cadr args)))
               ((f 'behaviour) (deref-list head s) s k))))

(primitive 'call/cc
           (lambda (args s k)
             (allocate
              1 s
              (lambda (a* ss)
                (let* ((addr (car a*))
                       (cc (create-function addr
                                            (lambda (argv s' _k)
                                              (k (car argv) s')))) ;; unwind mem?
                       (fn (car args)))
                  ((fn 'behaviour) (list cc) ss k))))))

                        
(define (repl)
  (let loop ((mem s.global))
    (evaluate (read) r.global mem
              (lambda (v mem')
                (display (object->meta v mem'))(newline)
                (loop mem')))))
