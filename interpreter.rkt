; Giuliana Conte gdc24
; Ellie Fitzpatrick eef33@case.edu
; Taylor Smith tps45

(require "simpleParser.scm") ; load parser


; Takes a filename, calls parser with the filename, evaluates the parse tree returned by parser,
; and returns the proper value.
; Maintains a state for the variables and returns an error message if the user attempts to use a
; variable before it is declared. Uses the Scheme function (error ...) to return the error.

(define interpret_parsetree
  (lambda (parsetree state)
    (if (null? parsetree)
        (if (state_member? 'return state)
            (state_lookup 'return state)
            (error "no return statement."))
        (interpret_parsetree (cdr parsetree) (m_state (car parsetree) state)))))


(define interpret
  (lambda (filename)
    (interpret_parsetree (parser filename) (state_new))))

    
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

; state_new creates an empty state
; parameters: none
(define state_new
  (lambda () (list empty_vars empty_vals)))

; add a binding to the state
; parameters: a variable, a value, and a state
(define state_add
  (lambda (var val state)
    (list (cons var (vars state)) (cons val (cadr state)))))

; remove a binding from the state
(define state_remove
  (lambda (var state)
    (cond
      ((null? (vars state)) state)
      ((eq? var (state_var1 state)) (state_cdrs state))
      (else (state_add (state_var1 state) (state_val1 state) (state_remove var (state_cdrs state)))))))

; returns true iff variable is in the state
(define state_member?
  (lambda (var state)
    (cond
      ((state_null? state) #f) ; if the state is empty, the variable is not in the state, so return #f
      ((eq? var (state_var1 state)) #t) ; if the var equals the first var in the state, return #t
      (else (state_member? var (state_cdrs state)))))) ; otherwise, perform the function on the state without the first binding
      
; returns true iff the state is empty
(define state_null?
  (lambda (state)
    (if (eq? 'null (vars state))  ; if there are no vriables in the state, then the state is empty
        #t
        #f)))     ; otherwise, the state is not null, so it returns false

; finds the value for the given variable
(define state_lookup
  (lambda (var state)
    (cond
      ((state_null? state) (error "No such variable")) ; if the state is null, there is no variable with the name that is being looked up, so throw an error
      ((eq? var (state_var1 state)) (state_val1 state)) ; check if the variable is the same as the state
      (else (state_lookup var (state_cdrs state)))))) ; otherwise, performs the lookup on the rest of the state

; returns the state without the first binding
(define state_cdrs
  (lambda (state)
    (list (cdar state) (cdadr state))))                                                                       ; returns two lists within a list, each one without their first element


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; m_state, m_value, and m_boolean functions to return the values from the parse tree                                ;
;                                                                                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; m_value takes an expression and a state and returns the value of the expression
(define m_value
  (lambda (expr state)
    (cond
      ((atom? expr) (m_value_atom expr state))
      (else (m_value_list expr state)))))

; m_boolean takes an expression and a state and returns the value of the expression
; (note: we handle booleans in the m_value functions, so that is why m_boolean and m_value are the same) 
(define m_boolean
  (lambda (expr state)
    (cond
      ((atom? expr) (m_value_atom expr state))
      (else (m_value_list expr state)))))

; m_state returns the updated state after the expression from the parse tree has been evaluated
(define m_state
  (lambda (expr state)
    (cond
      ((list? expr) (m_value_list expr state))
      (else (m_value_atom expr state)))))
      
(define m_state_statement
  (lambda (stmt state)
    (cond
      ((eq? 'if (car stmt)) (m_state_if stmt state))
      ((eq? 'var (car stmt)) (m_state_declare stmt state))
      ((eq? '= (car stmt)) ((m_state_assign stmt state)))
      ((eq? 'return (car stmt)) (toAtoms (state_add 'return (m_value (cadr stmt) state) (state_remove 'return state))))
      ((eq? 'while (car stmt)) (m_state_while stmt state)))))      
      
(define toAtoms
  (lambda (x)
    (cond
      ((eq? #t x) 'true)
      ((eq? #f x) 'false)
      (else x))))

(define atom?
  (lambda (x) 
    (and (not (pair? x))
       (not (null? x)))))
      

; Ellie Fitzpatrick
; eef33

; if statements


;If statement
; parameters: condition, then statment, else statement, and state
;if, then, and else statements
(define m_state_if
  (lambda (cond1 then-stmt else-stmt state)
    (if (m_bool cond1 state) (m_state then-stmt state))
    (else (m_state else-stmt state))))

; A simple if then statement
(define m_state_if
  (lambda (cond1 then-stmt state)
    (if (m_bool cond1) (m_state then-stmt state))))


; while statment
; parameters: while condition, loop body, state
(define m_state_while
  (lambda (cond1 body state)
    (if (m_bool cond1 state)
        (m_state (m_state_while cond1 body (m_state body state)))
        (m_state cond1 state))))

; return statement
; paramteters: what you want to return
(define m_state_return
  (lambda (x)
    (if (state_member? x state)
        (m_statelookup x state) ;if it is a variable, return the variable
        (m_value_math(x))))) ;if it is an expression, return the value of the expression

; This needs fixing because how do I deal with the potential for var x; and var x = 1;  without the expression list
; adds the variable 'var' to the vars list with a value of null
; parameters: the word var (if it is a declaration), variable
(define m_state_declare_assign
  (lambda (decl_stmt var_name value state)
    (state_add var_name value state)))))

(define m_state_declare
  (lambda (decl_stmt var_name state)
    (state_add decl_stmt null state)))
; This handles the situation x = 1;
;assigns a variable a value
; parameter: 
(define m_state_assign
  (lambda (var_name op value state)
    (if (eq? '= op)
        (if (state_member? var_name state)
            (state_add var_name value (state_remove var_name state)) ;if the variable is in the state, declare the variable
            (error("Variable not declared")))))) ;What do I do with this? 
        
        
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
        
; returns value of an assignment statement
(define m_value_statement
  (lambda (expr state)
    (if (or (eq? (car expr) '=) (eq? (car expr) 'var))
        (m_value (caddr expr) state)
        #f)))

; defines an operator that is a statement
(define statement '(var = if return))

; returns a value for part of the parse tree that is a list
(define m_value_list
  (lambda (expr state)
    (if (member? (car expr) statement)
        (m_value_statement expr state)
        (m_value_expression expr state))))

;member? returns true or false depending on if x is in the lis
; parameters: atom to find, lis to look in
(define member?
  (lambda (x lis)
    (if (null? lis) #f
        (if (equal? x (car lis)) #t
            (member? x (cdr lis))))))

; returns a value for part of the parse tree that is an atom
(define m_value_atom
  (lambda (expr state)
    (cond
      ((or (boolean? expr) (number? expr)) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      (else (state_lookup expr state)))))
