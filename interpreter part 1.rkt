(define mstate_new
  (lambda ()
    '(() ())))


(define vars car)

(define vals cadr)

(define mstate_var1 caar)

(define mstate_val1 caadr)

(define mstate_null?
  (lambda (state)
    (if (null? (vars state))
        #t
        #f)))

(define mstate_cdrs
  (lambda (state)
    (list (cdr (vars state)) (cdr (vals state)))))

(define mstate_add
  (lambda (var val state)
      (list (cons var (vars state)) (cons val (vals state)))))

;(define mstate_remove
;(lambda (var state)
;   (cond
;      ((mstate_null? state) (error "Variable not found"))
;      ((eq? var (mstate_var1 state)) (mstate_cdrs state))
;      (else (mstate_remove var (mstate_var1 state))))))

(define mstate_member?
  (lambda (var state)
    (cond
      ((mstate_null? state) #f)
      ((eq? (mstate_var1 state) var) #t)
      (else (mstate_member? var (mstate_cdrs state))))))

(define mstate_lookup
  (lambda (var state)
    (cond
      ((mstate_null? state) (error "Variable not found."))
      ((mstate_member? var state)