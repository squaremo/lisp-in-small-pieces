;; Procedures to show the various states of transformation.  This
;; covers the classes given in chapter9.ss; it is extended in
;; chapter10.ss

(define (show v)
  (cond ((or (instance-of? v <symbol>)
             (instance-of? v <number>)
             (instance-of? v <boolean>)
             (instance-of? v <string>)
             (instance-of? v <char>)
             (instance-of? v <procedure>))
         v)
        ((list? v)
         (map show v))
        ((vector? v)
         (vector-map show v))
        (else
         (show-class-value v))))

(define (show-class-value v)
  (let ((c (type-of v)))
    `(,(class-name c)
      ,@(map (lambda (s) (show-class-slot s v)) (class-direct-slots c)))))

(define (show-class-slot s v)
  `(,(slot-name s) ,(show ((slot-accessor s) v))))

(define-generics ->sexpr)

(define-methods ->sexpr
  ;; Sometimes we're in an implicit list-of-expressions and we want to
  ;; get something to cons to
  ([(<program> p) (<boolean> listy)]
   (if listy (list (->sexpr p)) (->sexpr p)))

  ([(<constant> c)] (:value c))
  ([(<variable> v)] (:name v))
  ([(<local-reference> r)] (-> r :variable ->sexpr))
  ([(<global-reference> r)] (-> r :variable ->sexpr))
  ([(<predefined-application> a)]
   `(,(-> a :variable :name) ,@(->sexpr (:arguments a))))
  ([(<predefined-reference> r)] (-> r :variable :name))
  ([(<some-arguments> a)]
   (cons (->sexpr (:first a)) (->sexpr (:others a))))
  ([(<no-argument> a)] '())
  ([(<fix-let> l)]
   `(let (,@(map (lambda (var expr) `(,(:name var) ,expr))
                 (:variables l) (->sexpr (:arguments l))))
      ,@(->sexpr (:body l) #t)))
  ([(<sequence> s)]
   `(begin
      ,(->sexpr (:first s))
      ,@(->sexpr (:last s) #t)))
  ([(<sequence> s) (<boolean> listy)]
   (if listy (cons (->sexpr (:first s) #f) (->sexpr (:last s) #t))
       (->sexpr s)))
  ([(<function> f)]
   `(lambda ,(map :name (:variables f))
      ,@(->sexpr (:body f) #t)))
  ([(<regular-application> a)]
   `(,(->sexpr (:function a)) ,@(->sexpr (:arguments a))))
  ([(<alternative> a)]
   `(if ,(->sexpr (:condition a))
        ,(->sexpr (:consequent a))
        ,(->sexpr (:alternant a))))
)
