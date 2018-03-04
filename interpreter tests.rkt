(load "interpreter.rkt")

(define tests
  (lambda ()
    (if (and (eq? (interpret "test1.txt") 150)
             (eq? (interpret "test2.txt") -4)
             (eq? (interpret "test3.txt") 10)
             (eq? (interpret "test4.txt") 16)
             (eq? (interpret "test5.txt") 220)
             (eq? (interpret "test6.txt") 5)
             (eq? (interpret "test7.txt") 6)
             (eq? (interpret "test8.txt") 10)
             (eq? (interpret "test9.txt") 5))
             ;(eq? (interpret "test10.txt") -39)
             ;(eq? (interpret "test11.txt") '"Variable not declared."))
        #t
        #f)))