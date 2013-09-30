;; The core 'meaning' procedures that get used from chapter 6 & 7
;; Requires prelude.ss, env.ss to be loaded

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

;; Conditional

(define (meaning-alternative ec et ef r tail?)
  (let ((mc (meaning ec r #f))
        (mt (meaning et r tail?))
        (mf (meaning ef r tail?)))
    (ALTERNATIVE mc mt mf)))

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
