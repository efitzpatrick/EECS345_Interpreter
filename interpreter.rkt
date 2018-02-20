; Giuliana Conte gdc24
; Ellie Fitzpatrick eef33

(require "simpleParser.scm")         ; load parser


; Takes a filename, calls parser with the filename, evaluates the parse tree returned by parser,
; and returns the proper value.
; Maintains a state for the variables and returns an error message if the user attempts to use a
; variable before it is declared. Uses the Scheme function (error ...) to return the error.

(define interpret
  (lambda (filename)
    (parser (filename))));
    
; defining commonly used words for abstraction
(define vars car)           ; list of variables in the state
(define vals cdr)           ; list of values in the state
(define state_var1 caar)   ; first variable in the state
(define state_val1 caadr)  ; first value in the state
(define empty_vars (list))  ; empty list of variables
(define empty_vals (list))  ; empty list of values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; interacting with the state functions. implemented in two lists:                                                   ;
; first list is all the variable names and second list is all the values, since that will be easier for the future  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; creates an empty state
(define state_new
  (lambda () (list empty_vars empty_vals)))                                                                   ; create a new state that contains a list, one of variables and one of values

; add a binding to the state
(define state_add
  (lambda (var val state)                                                                                     ; takes a variable, a value, and a state
    (list (cons var (vars state)) (cons val (cadr state)))))                                                  ; add the variable to the list of variables; also add the value to the list of values

; remove a binding from the state
(define state_remove
  (lambda (var state)
    (cond
      ((null? (vars state)) state)                                                                            ; if the state is null, return the empty state
      ((eq? var (state_var1 state)) (state_cdrs state))                                                       ; if the first variable of the state equals the variable to be removed, then return the rest of the state without that binding
      (else (state_add (state_var1 state) (state_val1 state) (state_remove var (state_cdrs state)))))))       ; otherwise, add the first binding to the new state and call the function on cdrs

; returns true iff variable is in the state
(define state_member?
  (lambda (var state)
    (cond
      ((state_null? state) #f)
      ((eq? var (state_var1 state)) #t)
      (else (state_member? var (state_cdrs state))))))
      
; returns true iff the state is empty
(define state_null?
  (lambda (state)
    (if
      (null? (vars state))
      #t
      #f)))

; finds the value for the given variable
(define state_lookup
  (lambda (var state)
    (cond
      ((state_null? state) (error "No such variable"))
      ((eq? var (state_var1 state)) (state_val1 state))
      (else (state_lookup var (state_cdrs state))))))

; returns the state without the first binding
(define state_cdrs
  (lambda (state)
    (list (cdar state) (cdadr state))))

; Ellie Fitzpatrick
; eef33

; if statements


;3 variable (if, then-stmt, else-stmt)
(define m_state_if
  (lambda (cond1 then_stmt else_stmt)
    (if (m_bool(cond1)) then_stmt)
    ( else_stmt)))

;2 variable (if, then-stmt)
(define m_state_if
  (lambda (cond1 then_stmt)
    (if (m_bool(cond1)) then_stmt)))

; while statments
; i need to use the tail end recursion, I know I did not implement this correctly
(define m_state_while
  (lambda (cond1 then_stmt state)
    (if (m_bool(cond1))
        (m_state(while_stmt(cond1 then_stmt m_state(then_stmt state)))))
    (mstate(cond1 state))))

; return statement
(define m_state_while
  (lambda (x)
    (if (m_state_member(x state)) (m_statelookup(x state))) ;if it is a variable, return the variable
    (m_value_math(x)))) ;if it is an expression, return the value of the expression
    
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
    

