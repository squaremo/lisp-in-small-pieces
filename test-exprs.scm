;; Test expressions for run-smoketest.scm

"built-in in head position, literals"
(+ 1 1)

"let form"
((lambda (x y) (+ y x)) 1 2)

"abstraction and application"
((lambda (f) (f 2 2)) (lambda (x y) (+ x y)))

"dotted let, built-in as value, and apply"
((lambda (f . args) (apply f args)) + 2 3)

"list primitive"
(list 1 2 3)

"list and apply"
(apply + (list 3 4))

"call/cc"
(+ 4 (call/cc (lambda (k) (k 4))))
