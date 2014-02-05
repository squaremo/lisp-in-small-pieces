;; Procedures to show the various states of transformation


(define-generics ->sexpr)

(define-macro (-> value . rest)
  (cond
   ((null? rest)
    value)
   ((pair? rest)
    (let ((next (car rest)))
      (if (pair? next)
          `(-> (,(car next) ,value ,@(cdr next)) ,@(cdr rest))
          `(-> (,next ,value) ,@(cdr rest)))))))

(define-methods ->sexpr
  ;; Sometimes we're in an implicit list-of-expressions and we want to
  ;; get something to cons to
  ([(<program> p) (<boolean> listy)]
   (if listy (list (->sexpr p)) (->sexpr p)))

  ([(<constant> c)] (:value c))
  ([(<variable> v)] (:name v))
  ([(<local-reference> r)] (-> r :variable :name))
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