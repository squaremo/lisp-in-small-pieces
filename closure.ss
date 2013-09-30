;; OO definition of closures, used starting with chapter 6.

;; Closures are back to being objects rather than erm, closures.

(define-generics :code :code! :closed-env :closed-env!)

(define-class (<closure>)
  (code :code :code!)
  (closed-env :closed-env :closed-env!))

;; I'm going to use a generic procedure here, as well, again for the
;; effect of checking the type of argument it's given.
(define-generics invoke)
