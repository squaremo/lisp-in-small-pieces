;; This compiler uses the expander of the previous (and the
;; 'objectification'), then walks the resulting object tree and emits
;; C.

(load "chapter9.ss")

;; We start by making a code walker: it'll visit each subprogram and
;; thereby construct a new program.


;; I'm going to take a slightly different route to the book, by
;;  1. using a generic procedure to pick the fields to visit, rather
;;  than reflection
;;  2. constructing a new tree rather than setting fields
;;
;; The supplied procedure gets to see each subprogram first; if it
;; chooses to walk the subprogram (i.e., it's not of interest, but
;; sub-sub-programs might be) then each program-like field will be
;; examined.

(define (walk fun program . args)
  (visit program (if (null? args)
                     fun
                     (lambda (p) (apply fun p args)))))

(define-generics visit)

;; Some programs don't have any fields that are programs; stop walking
(define-method (visit (<program> p)
                      (<procedure> f))
  p)

(define-method (visit (<global-assignment> assign)
                      (<procedure> fun))
  (make <global-assignment> (:variable assign)
        (fun (:form assign))))
(define-method (visit (<local-assignment> assign)
                      (<procedure> fun))
  (make <local-assignment> (fun (:reference assign))
        (fun (:form assign))))
(define-method (visit (<function> f)
                      (<procedure> fun))
  (make <function> (:variables f)
        (fun (:body f))))
(define-method (visit (<alternative> alt)
                      (<procedure> fun))
  (make <alternative> (fun (:condition alt))
        (fun (:consequent alt))
        (fun (:alternant alt))))
(define-method (visit (<sequence> seq)
                      (<procedure> fun))
  (make <sequence> (fun (:first seq))
        (fun (:last seq))))
(define-method (visit (<some-arguments> args)
                      (<procedure> fun))
  (make <some-arguments> (fun (:first args))
        (fun (:others args))))
(define-method (visit (<regular-application> app)
                      (<procedure> fun))
  (make <regular-application>
    (fun (:function app)) (fun (:arguments app))))
(define-method (visit (<predefined-application> app)
                      (<procedure> fun))
  (make <predefined-application> (:variable app)
        (fun (:arguments app))))
(define-method (visit (<fix-let> fix)
                      (<procedure> fun))
  (make <fix-let> (:variables fix)
        (fun (:arguments fix))
        (fun (:body fix))))

;; Here's the canonical example of a walker:
(define (identity p) (walk identity p))

;; === Using boxes for mutable variables

(define-generics insert-boxes)

(define-method (insert-boxes (<program> p))
  (walk insert-boxes p))

;; A few more classes to represent box reads, writes, and the creation
;; of boxes.
(define-class (<box-read> <program>)
  (reference :reference :reference!))
(define-method (initialize (<box-read> self)
                           (<reference> ref))
  (init* self :reference! ref))
(define-method (visit (<box-read> read)
                      (<procedure> fun))
  (make <box-read> (fun (:reference read))))

(define-class (<box-write> <program>)
  (reference :reference :reference!)
  (form :form :form!))
(define-method (initialize (<box-write> self)
                           (<reference> ref)
                           (<program> form))
  (init* self :reference! ref :form! form))
(define-method (visit (<box-write> write)
                      (<procedure> fun))
  (make <box-write> (fun (:reference write))
        (fun (:form write))))

(define-class (<box-creation> <program>)
  (variable :variable :variable!))
(define-method (initialize (<box-creation> self)
                           (<variable> var))
  (init* self :variable! var))

;; And now for the specialisations that we care about:
(define-method (insert-boxes (<local-reference> ref))
  (if (:mutable? (:variable ref))
      (make <box-read> ref)
      ref))

;; Rewrite assignments to locals as box-writes. Note that I don't use
;; walk, because I'm recursing on a field that I know is a program;
;; that's going to be the case in general.
(define-method (insert-boxes (<local-assignment> set))
  (make <box-write> (:reference set) (insert-boxes (:form set))))

(define-method (insert-boxes (<function> f))
  (let ((body (boxify-mutable-variables (:body f)
                                        (:variables f))))
    (make <function> (:variables f) body)))

(define-method (insert-boxes (<fix-let> f))
  (let ((body (boxify-mutable-variables (:body f)
                                        (:variables f)))
        (args (insert-boxes (:arguments f))))
    (make <fix-let> args body)))

(define (boxify-mutable-variables body vars)
  (if (pair? vars)
      (if (:mutable? (car vars))
          (boxify-mutable-variables
           (make <sequence> (make <box-creation> (car vars)) body))
          (boxify-mutable-variables body (cdr vars)))
      body))


;; === Lambda-lifting

;; This transforms abstractions in-place, from those closing over free
;; variables to those taking a flattened, free (variable) environment
;; as well as their arguments. Subsequent transformations will bubble
;; these up to the top of the program.

;; The entry point
(define (lambda-lift p)
  (lift-procedures p #f '()))

;; Classes to represent functions with flattened environments.

;; Same abstract super thing here as for <arguments>
(define-class (<free-environment> <program>))

(define-generics :free :free!)
(define-class (<flat-function> <function>)
  (free :free :free!))
(define-method (initialize (<flat-function> self)
                           (<list> vars)
                           (<program> body)
                           (<free-environment> free))
  (init* self :variables! vars :body! body :free! free))
(define-method (visit (<flat-function> f)
                      (<procedure> fun))
  (make <flat-function> (:variables f)
        (fun (:body f)) (fun (:free f))))


(define-class (<some-free> <free-environment>)
  (first :first :first!)
  (others :others :others!))
(define-method (initialize (<some-free> self)
                           (<program> first)
                           (<free-environment> others))
  (init* self :first! first :others! others))
(define-method (visit (<some-free> env)
                      (<procedure> fun))
  (make <some-free> (fun (:first env)) (fun (:others env))))

(define-class (<no-free> <free-environment>))

(define-class (<free-reference> <reference>))

(define-generics lift-procedures)

(define-method (lift-procedures (<program> p)
                                (<value> f) ; <flat-function> | #f
                                (<list> vars))
  (walk lift-procedures p f vars))

;; If a local reference is not in the var list for this abstraction,
;; it's free, so put it in the free-environment and replace it with a
;; free-reference. I use a bit of mutation here, but it operates only
;; on things created during the tree walk.
(define-method (lift-procedures (<local-reference> ref)
                                (<value> f) ; <flat-function> | #f
                                (<list> vars))
  (let ((v (:variable ref)))
    (if (memq v vars)
        ref
        (begin
          (adjoin-free-variable! f ref)
          (make <free-reference> v)))))
;; Add a free variable to the free-environment if it's not there
;; already
(define (adjoin-free-variable! flat ref)
  (when (instance-of? flat <flat-function>)
        (let check ((free* (:free flat)))
          (if (instance-of? free* <no-free>)
              (:free! flat (make <some-free> ref (:free flat)))
              (unless (eq? (:variable ref)
                           (:variable (:first free*)))
                      (check (:others free*)))))))

(define-method (lift-procedures (<fix-let> fix)
                                (<value> f)
                                (<list> vars))
  (let ((newvars (append (:variables fix) vars)))
    (make <fix-let> vars
          (lift-procedures (:arguments fix) f vars)
          (list-procedures (:body fix) f newvars))))

;; Again, a bit of mutation, because we construct the flat-fun with
;; the 'original' body as a placeholder, then process the body with
;; itself as the container for the free-environment, *then* process
;; the free environment of the new abstraction (which now contains the
;; vars free in that abstraction) in terms of the surrounding
;; abstraction, to collect those vars already free outside the new
;; abstraction.
(define-method (lift-procedures (<function> fun)
                                (<value> f)
                                (<list> vars))
  (let* ((localvars (:variables fun))
         (body (:body fun))
         (newf (make <flat-function> localvars body (make <no-free>))))
    (:body! newf (lift-procedures body newf localvars))
    ;; reprocess the free variables in terms of the current abstraction
    (:free newf (lift-procedures (:free newf) f vars))
    newf))

;; === Collect quotations and functions

(define-generics :quotations :quotations! :definitions :definitions!)

(define-class (<flat-program> <program>)
  (form :form :form!)
  (quotations :quotations :quotations!)
  (definitions :definitions :definitions!))
(define-method (initialize (<flat-program> self)
                           (<program> form)
                           (<list> quotes)
                           (<list> defs))
  (init* self :form! form :quotations! quotes :definitions! defs))
(define-method (visit (<flat-program> flat)
                      (<procedure> fun))
  (make <flat-program> (:quotes flat)
        (map fun (:definitions flat))))

;; NB uses the `name` slot to store the index
(define-class (<quotation-variable> <variable>)
  (value :value :value!))
(define-method (initialize (<quotation-variable> self)
                           (<number> index)
                           (<value> value))
  (init* self :name! index :value! value))

;; An extracted, lifted lambda. NB 'free' is *not* a free environment
;; here, but merely a list of variables free in the body.
(define-generics :index :index!)
(define-class (<function-definition> <flat-function>)
  (index :index :index!))
(define-method (initialize (<function-definition> self)
                           (<list> vars)
                           (<program> body)
                           (<list> free)
                           (<number> index))
  (init* self :variables! vars :body! body :free! free :index! index))
(define-method (visit (<function-definition> def)
                      (<procedure> fun))
  (make <function-definition>
    (:variables def) (fun (:body def)) (:free def) (:index def)))

;; This will now stand in for abstractions that have been lifted; now
;; we inherit the free var environment from the flat-function, but
;; refer to the function definition.
(define-class (<closure-creation> <program>)
  (index :index :index!)
  (variables :variables :variables!)
  (free :free :free!))
(define-method (initialize (<closure-creation> self)
                           (<number> index)
                           (<list> vars)
                           (<free-environment> free))
  (init* self :index! index :variables! vars :free! free))
(define-method (visit (<closure-creation> c)
                      (<procedure> fun))
  (make <closure-creation> (:index c) (:variables c)
        (fun (:free c))))

;; Now the entry point. Again, this uses mutation on something created
;; for the transformation. The reason is similar to above, we need to
;; create both a reference and the container to collect things into.
(define (extract-things p)
  (let ((result (make <flat-program> p '() '())))
    (:form! result (extract p result))
    result))

(define-generics extract)

(define-method (extract (<program> p)
                        (<flat-program> top))
  (walk extract p top))

(define-method (extract (<constant> c)
                        (<flat-program> top))
  (let* ((qv* (:quotations top))
         (qv (make <quotation-variable> (length qv*) (:value c))))
    (:quotations! top (cons qv qv*))
    (make <global-reference> qv)))

(define-method (extract (<flat-function> f)
                        (<flat-program> top))
  ;; First construct a new flat-function given what we know
  (let* ((newbody (extract (:body f) top))
         (vars (:variables f))
         (freevars (let extr ((free (:free f)))
                     (if (instance-of? free <some-free>)
                         (cons (:variable (:first free))
                               (extr (:others free)))
                         '())))
         (index (adjoin-definition! top vars newbody freevars)))
    (make <closure-creation> index vars (:free f))))

(define (adjoin-definition! top vars body free)
  (let* ((defs (:definitions top))
         (newindex (length defs)))
    (:definitions!
     top
     (cons (make <function-definition> vars body free newindex)
           defs))
    newindex))

;; Lastly, the wole program gets made into a thunk. (I like this name
;; better than that of the book)
(define (thunkify-main top)
  (let ((index (length (:definitions top))))
    (:definitions! top
                   (cons (make <function-definition>
                           '() (:form top) '() index)
                         (:definitions top)))
    (:form! top
            (make <regular-application>
              (make <closure-creation> index '() (make <no-free>))
              (make <no-argument>)))
    top))
