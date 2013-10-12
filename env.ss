(import type-system)
(import generic-procedures)
(import oo)

;; Activation records: I'm collapsing the frankly profligate two
;; classes (environment, not used qua itself, and activation) from the
;; chapter5 interpreter into just the one. We're still using the
;; nested environments.

;; I'm exercising the type system a little more than the book does,
;; just by defining some procedures as generic procedures for the
;; effect of checking the arguments passed match the signature given.

(define-generics
  :next :next!
  :args :args!
  :argument :argument!
  :length
  extend ;; sr-extend* in the book
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


;; Helper that collects values after arity+1 and puts them in a list
;; in arity+1th argument slot. Used for dotted applications.
(define (listify! v* arity)
  (let loop ((index (- (:length v*) 1))
             (result '()))
    (if (= arity index)
        (:argument! v* arity result)
        (loop (- index 1)
              (cons (:argument v* (- index 1)) result)))))


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
