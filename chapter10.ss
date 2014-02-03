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

(define (walk program fun . args)
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
(define (identity p) (walk p identity))

