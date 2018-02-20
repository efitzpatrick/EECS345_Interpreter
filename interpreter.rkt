; Giuliana Conte gdc24
; Ellie Fitzpatrick eef33
; Taylor Smith tps45

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
(define state_var1 caar)    ; first variable in the state
(define state_val1 caadr)   ; first value in the state
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  m_state, m_value, and m_boolean                                                                                  ;
;                                                                                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; m_value takes an expression and a state and returns the value of the expression
(define m_value
  (lambda (expr state)
    (cond
      ((atom? expr) m_value_atom expr state)
      (else m_value_list expr state))))

; m_boolean takes an expression and a state and returns the value of the expression
; (note: we handle booleans in the m_value functions, so that is why m_boolean and m_value are the same) 
(define m_boolean
  (lambda (expr state)
    (cond
      ((atom? expr) m_value_atom expr state)
      (else m_value_list expr state))))

; m_state returns the updated state after the parse tree has been evaluated
(define m_state
  (lambda (expr state)
    (cond
      ((list? expr) (m_state_list expr state))
      (else (m_state_atom expr state)))))
      

; Ellie Fitzpatrick
; eef33

; if statements


;3 variable (if, then-stmt, else-stmt)and state
(define m_state_if
  (lambda (expression state)
    (if (m_bool((cadr expression) state) m_state((caddr lis) state))
    ( m_state((cadddr lis) state)))))

;2 variable (if, then-stmt)
(define m_state_if
  (lambda (expression state)
    (if (m_bool(cadr expression)) m_state((caddr lis) state))))

; while statments
; i need to use the tail end recursion, I know I did not implement this correctly
(define m_state_while
  (lambda (expression state)
    (if (m_bool((cadr expression) state))
        (m_state(while_stmt(expression m_state(then_stmt state)))))
    (mstate(cond1 state))))

; return statement
(define m_state_return
  (lambda (x)
    (if (m_state_member(x state)) (m_statelookup(x state))) ;if it is a variable, return the variable
    (m_value_math(x)))) ;if it is an expression, return the value of the expression

; adds the variable 'var' to the vars list with a value of null
; if expression has a len of 2
(define m_state_declare
  (lambda (expression state)
      (if 'var (car expression)
          (if eq? ('= (caddr expression))
              (state_Add (cadr expression) (cdddr expression) state)) ;if a declaration and assignment, declare and assign the value
          (m_add((cadr expression) null state))))) ;otherwise, just declare the values with a null as the value
           
;if the expression has a len of 3
(define m_state_assign
  (lambda (expression state)
    (if (eq? '= (cadr expression))
        (if (state_member? (car expression) state) (state_add (car expression) (cddr expression) (state_remove (car expression) state))) ;if the variable is in the state, declare the variable
        (error("Variable not declared")))))
        
        
; Taylor Smith tps45

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define binary-ops
  (lambda (op)
    (cond
      ((eq? op '+) +)
      ((eq? op '-) -)
      ((eq? op '*) *)
      ((eq? op '/) quotient)
      ((eq? op '%) remainder)
      ((eq? '<) <)
      ((eq? '>) >)
      ((eq? '<=) <=)
      ((eq? '>=) >=)
      ((eq? '==) =)
      ((eq? '!=) !=)
      ((eq? op '&&) (lambda (x y) (and x y)))
      ((eq? op '||) (lambda (x y) (or x y))))))

(define !=
  (lambda (x y)
    (not (= x y))))

(define unary-ops
  (lambda (op)
    (cond
      ((eq? op '!) not)
      ((eq? op '-) (lambda (x) (- 0 x))))))

; returns the value of an arithemtic expression whether the operator is unary or binary
(define m_value_expression
  (lambda (expr state)
    (if (eq? 3 (length expr))
        ((binary-ops (operator expr))(m_value (operand1 expr) state) (m_value (operand2 expr) state))
        ((unary-ops (operator expr) (m_value (operand1 expr) state))))))
        
