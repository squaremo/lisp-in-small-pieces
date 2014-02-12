;; Test expressions for run-smoketest.scm

"built-in in head position, literals"
(+ 1 1)

"let form"
((lambda (x y) (+ y x)) 1 2)

"abstraction and application"
((lambda (f) (f 2 2)) (lambda (x y) (+ x y)))

"thunk let"
((lambda () (+ 1 2)))

"thunk application"
((lambda (f) (f)) (lambda () (+ 2 3)))

"dotted let, built-in as value, and apply"
((lambda (f . args) (apply f args)) + 2 3)

"list primitive"
(list 1 2 3)

"list and apply"
(apply + (list 3 4))

"call/cc"
(+ 4 (call/cc (lambda (k) (k 4))))

"nested let with call/cc"
((lambda (a p b)
   ((lambda (c)
      (p (call/cc c) b))
    (lambda (k) (k a)))) 1 + 2)

"set! in fix-let"
((lambda (a) (set! a 1) a) #f)

"set! global variable"
(begin (set! b "foobar") 2)

"set! in function"
((lambda (f) (f #f 1)) (lambda (a b) (set! a b) a))
