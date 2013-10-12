(+ 1 1)
((lambda (x y) (+ y x)) 1 2)
((lambda (f . args) (apply f args)) + 1 3)
