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
(define-method (visit (<arguments> args)
                      (<procedure> fun))
  (make <arguments> (fun (:first args))
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
;; walk, because I'm recursing on a field; that's going to be the case
;; in general.
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
