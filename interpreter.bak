; Giuliana Conte
; gdc24

(require "simpleParser.scm")         ; load parser


; Takes a filename, calls parser with the filename, evaluates the parse tree returned by parser,
; and returns the proper value.
; Maintains a state for the variables and returns an error message if the user attempts to use a
; variable before it is declared. Uses the Scheme function (error ...) to return the error.

(define interpret
  (lambda (filename)
    (parser (filename))));
    


; all state functions. implemented in two lists:
; first list is all the variable names and second list is all the values, since that will be easier for the future

; creates an empty state
(define mstate_new
  (lambda () '(() ())))

; list of variables in the state
(define vars car)

; list of values in the state
(define vals cdr)

; first variable in the state
(define mstate_var1 caar)

; first value in the state
(define mstate_val1 caadr)

; add a binding to the state
(define mstate_add
  (lambda (var val state)
    (list (cons var (vars state)) (cons val (cadr state)))))

; remove a binding from the state
(define mstate_remove
  (lambda (var state)
    (cond
      ((null? (vars state)) state)                                                                            ; if the state is null, return the empty state
      ((eq? var (mstate_var1 state)) (mstate_cdrs state))                                                     ; if the first variable of the state equals the variable to be removed, then return the rest of the state without that binding
      (else (mstate_add (mstate_var1 state) (mstate_val1 state) (mstate_remove var (mstate_cdrs state)))))))  ; otherwise, add the first binding to the new state and call the function on cdrs

; returns true iff variable is in the state
(define mstate_member?
  (lambda (var state)
    (cond
      ((mstate_null? state) #f)
      ((eq? var (mstate_var1 state)) #t)
      (else (mstate_member? var (mstate_cdrs state))))))
      
      
; returns true iff the state is empty
(define mstate_null?
  (lambda (state)
    (if
      (null? (vars state))
      #t
      #f)))

; finds the value for the given variable
(define mstate_lookup
  (lambda (var state)
    (cond
      ((mstate_null? state) (error "No such variable"))
      ((eq? var (mstate_var1 state)) (mstate_val1 state))
      (else (mstate_lookup var (mstate_cdrs state))))))

; returns the state without the first binding
(define mstate_cdrs
  (lambda (state)
    (list (cdar state) (cdadr state))))

      
      

; Ellie Fitzpatrick
; eef33

; if statements

(define m_state_if
  (lambda (cond1 then_stmt else_stmt)
    (m_bool(cond1)
           then_stmt
           else_stmt)))

; while statments
; i need to use the tail end recursion, I know I did not implement this correctly
(define while_stmt
  (lambda (cond1 then_stmt break)
    ((m_bool(cond1)) (while_stmt(cond1 then_stmt)))
    (else break)))
    
; Taylor Smith tps45
; helper functions 
(define operator
  (lambda (x)
    (cadr x)))

(define operand1 car)

(define operand2 caddr)

; M_value_math takes the mathematical operators +,-,*,/,% and evaluates in scheme
(define M_value_math
  (lambda (x)
    (cond
      ((number? x) x)
      ((eq? '+ (operator x)) (+ (M_value_math (operand1 x))(M_value_math (operand2 x))))
      ((eq? '- (operator x)) (- (M_value_math (operand1 x)) (M_value_math(operand2 x))))
      ((eq? '* (operator x)) (* (M_value_math (operand1 x)) (M_value_math(operand2 x))))
      ((eq? '/ (operator x)) (quotient (M_value_math (operand1 x)) (M_value_math(operand2 x))))
      ((eq? '% (operator x)) (remainder (M_value_math (operand1 x)) (M_value_math(operand2 x))))
      (else (error 'badop "Undefined operator")))))

; M_value_comp takes the comparison operators <,>,<=,>=,==,!= and evaluates in scheme
(define M_value_comp
  (lambda (x)
    (cond
      ((eq? '< (operator x)) (< (M_value_comp (operand1 x)) (M_value_comp (operand2 x))))
      ((eq? '> (operator x)) (> (M_value_comp (operand1 x)) (M_value_comp (operand2 x))))
      ((eq? '<= (operator x)) (<= (M_value_comp (operand1 x)) (M_value_comp (operand2 x))))
      ((eq? '>= (operator x)) (>= (M_value_comp (operand1 x)) (M_value_comp (operand2 x))))
      ((eq? '== (operator x)) (eq? (M_value_comp (operand1 x)) (M_value_comp (operand2 x))))
      ((eq? '!= (operator x)) (not (eq? ((M_value_comp (operand1 x)) (M_value_comp (operand2 x)))))))))

; M_value_bool takes the boolean operators && and || and evaluates in scheme
(define M_value_bool
  (lambda (x)
    (cond
      ((eq? '&& (operator x)) (and (M_value_bool (operand1 x)) (M_value_bool (operand2 x))))
      ((eq? '|| (operator x)) (or (M_value_bool (operand1 x)) (M_value_bool (operand2 x)))))))
    

