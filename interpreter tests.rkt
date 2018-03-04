(load "interpreter.rkt")

(define tests
  (lambda ()
    (if (and (eq? (interpret "test1.txt") 150)
             (eq? (interpret "test2.txt") -4)
             (eq? (interpret "test3.txt") 10)
             (eq? (interpret "test4.txt") 16)
             (eq? (interpret "test5.txt") 220))
        #t
        #f)))