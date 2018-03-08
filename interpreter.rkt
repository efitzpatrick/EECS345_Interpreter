; Giuliana Conte gdc24
; Ellie Fitzpatrick eef33
; Taylor Smith tps45

(require "simpleParser.scm") ; load parser


; Takes a filename, calls parser with the filename, evaluates the parse tree returned by parser,
; and returns the proper value.
; Maintains a state for the variables and returns an error message if the user attempts to use a
; variable before it is declared. Uses the Scheme function (error ...) to return the error.

(define interpret_parsetree
  (lambda (parsetree state)
    (if (null? parsetree)
        (if (eq? 'return (var1 (first_layer state)))
            (state_lookup 'return state)
            (error "no return statement."))
        (interpret_parsetree (cdr parsetree) (m_state (car parsetree) state)))))

(define interpret
  (lambda (filename)
    (interpret_parsetree (parser filename) (state_new))))

; defining commonly used words for abstraction
(define vars car)           ; list of variables in the state
(define vals cdr)           ; list of values in the state
(define var1 caar)          ; first variable in the state
(define val1 caadr)         ; first value in the state
(define empty_vars (list))  ; empty list of variables
(define empty_vals (list))  ; empty list of values
(define list_of_vals cadr)  ; list of the values
(define var_cdrs cdar)      ; all varables except the first
(define val_cdrs cdadr)     ; all values except the first

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; interacting with the state functions. implemented in two lists:                                                   ;
; first list is all the variable names and second list is all the values, since that will be easier for the future  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define first_layer car)
(define rest_of_layers cdr)

; state_new creates an empty state
; parameters: none
(define state_new
  (lambda () (list (new_layer))))

; add a layer to the state
; parameter: a state
(define add_layer
  (lambda (state)
    (cons (new_layer) state)))

; remove a layer from the state
; parameter: a state
(define remove_layer
  (lambda (state)
    (if (eq? state '())
        (error "No layers to remove")
        (cdr state))))

; add a binding to the top layer of the state
; parameters: a variable, a value, and a staet
(define state_add
  (lambda (var val state)
    (cons (add_to_layer var val (first_layer state)) (rest_of_layers state))))

; remove a binding from a layer
; parameters: a variable and a layer
(define state_remove
  (lambda (var state)
    (cond
      ((state_null? state) (error "Variable not found"))
      ((layer_member? var (first_layer state))
       (list (remove_from_layer var (first_layer state)) (rest_of_layers state)))
      (else (state_remove var (rest_of_layers state))))))

(define remove_from_layer
  (lambda (var layer)
    (cond
      ((null? (vars layer)) layer) ; if it's null, just return the layer
      ((eq? var (var1 layer)) (layer_cdrs layer)) ; if it is the first variable, return the rest of the layer
      (else (add_to_layer (var1 layer) (val1 layer) (remove_from_layer var (layer_cdrs layer)))))))

; returns true iff variable is in the state
; parameters: variable and state
(define state_member?
  (lambda (var state)
    (cond
      ((state_null? state) #f)
      ((eq? 'no_such_var (layer_lookup var (first_layer state)))
       (state_member? var (rest_of_layers state)))
      (else #t))))
      
; returns true iff the state is empty
; parameters: a state
(define state_null?
  (lambda (state)
    (if (null? state)
        #t
        #f)))

; returns the value of the given variable
; parameters: a variable and the state
(define state_lookup
  (lambda (var state)
    (cond
      ((state_null? state) (error "No such variable"))
      ((layer_empty? (first_layer state)) (layer_lookup var (rest_of_layers state)))
      ((eq? var (var1 (first_layer state)))
       (layer_lookup var (first_layer state)))
      ((not (eq? var (var1 (first_layer state)))) ; check the rest of layer 1
       (layer_lookup var (layer_cdrs (first_layer state))))
;      ((eq? var (layer_lookup var (first_layer state))) ; no variable in the first layer
;       (state_lookup var (rest_of_layers state)))
      (else (layer_lookup var (rest_of_layers state))))))

; returns the state without the first binding
; parameters: a state
(define state_cdrs
  (lambda (state)
    (list (var_cdrs state) (val_cdrs state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Layer functions                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define first_layer_vars caar)
(define first_layer_vals cadar)

; returns a new layer
; parameters: none
(define new_layer
  (lambda ()
    (list empty_vars empty_vals)))

; returns true iff there are no variables stored in the layer
; parameters: a layer
(define layer_empty?
  (lambda (layer)
    (null? (car layer))))

; add a binding to the layer
; parameters: a variable, a value, and a layer
(define add_to_layer
  (lambda (var val layer)
    (list (cons var (vars layer)) (cons val (list_of_vals layer)))))

; remove a binding from a layer
; parameters: a variable and a layer
;(define remove_from_layer
;  (lambda (var layer)
;    (cond
;      ((null? (vars layer)) layer)
;      ((eq? var (var1 layer)) (layer_cdrs layer))
;      (else (add_to_layer (var1 layer) (val1 layer) (remove_from_layer var (layer_cdrs layer)))))))

; returns the layer without the first binding
; parameters: a layer
(define layer_cdrs
  (lambda (layer)
    (list (var_cdrs layer) (val_cdrs layer))))

; returns the value of the given variable
; parameters: a variable and a layer
(define layer_lookup
  (lambda (var layer)
    (cond
      ((layer_empty? layer) 'no_such_var)
      ((eq? var (var1 layer)) (val1 layer))
      (else (layer_lookup var (layer_cdrs layer))))))

; returns true iff variable is in the layer
; parameters: a variable and a layer
(define layer_member?
  (lambda (var layer)
    (cond
      ((layer_empty? layer) #f)
      ((eq? var (var1 layer)) #t)
      (else (layer_member? var (layer_cdrs layer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; m_state, m_value, and m_boolean functions to return the values from the parse tree                                ;
;                                                                                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; m_value takes an expression and a state and returns the value of the expression
; parameters: takes an expression and a state
(define m_value
  (lambda (expr state)
    (if (atom? expr)
        (m_value_atom expr state)
        (m_value_list expr state))))

; m_boolean takes an expression and a state and returns the value of the expression
; parameters: an expression and a state
; (note: we handle booleans in the m_value functions, so that is why m_boolean and m_value are the same)
(define m_boolean
  (lambda (expr state)
    (if (atom? expr)
        (m_value_atom expr state)
        (m_value_list expr state))))

; m_state returns the updated state after the expression from the parse tree has been evaluated
; parameters: expression
(define m_state
  (lambda (expr state)
    (m_state_statement expr state)))
;    (cond
 ;     ((list? expr) (m_value_list expr state))
  ;    (else (m_value_atom expr state)))))

(define stmt_type car) ; statement type, i.e. if, while, etc
(define empty_when_no_else cdddr) ; if the if statement has no else block, this will be an empty list
(define empty_when_only_assigning cddr) ; if the statement is only assigning and not declaring, this will be an empty list
(define declared_var cadr)
(define assigned_val caddr)
      
(define m_state_statement
  (lambda (stmt state)
    (cond
      ((and (eq? 'if (stmt_type stmt))
            (not (eq? (empty_when_no_else stmt) '())))
       (m_state_if_else (cond1 stmt) (then-stmt stmt) (else-stmt stmt) state))
      ((eq? 'if (stmt_type stmt)) (m_state_if (cond1 stmt) (then-stmt stmt) state))
      ((and (eq? 'var (stmt_type stmt))
            (not (eq? (empty_when_only_assigning stmt) '())))
       (m_state_declare_assign (declared_var stmt) (assigned_val stmt) state))
      ((eq? '= (stmt_type stmt)) (m_state_assign (declared_var stmt) (assigned_val stmt) state))
      ((eq? 'var (stmt_type stmt)) (m_state_declare (declared_var stmt) state))
      ((eq? 'return (stmt_type stmt)) (toAtoms (state_add 'return (return_helper (m_value (declared_var stmt) state)) state))); (state_remove 'return state))))
      ((eq? 'while (stmt_type stmt)) (m_state_while (cond1 stmt) (then-stmt stmt) state (lambda (v) v))))))      
      
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

(define cond1 cadr)
(define then-stmt caddr)
(define else-stmt cadddr)

;If statement
; parameters: condition, then statment, else statement, and state
; if, then, and else statements
(define m_state_if_else
  (lambda (cond1 then-stmt else-stmt state)
    (if (m_boolean cond1 state)
        (m_state then-stmt state)
        (m_state else-stmt state))))

; A simple if then statement
(define m_state_if
  (lambda (cond1 then-stmt state)
    (if (m_boolean cond1 state)
        (m_state then-stmt state)
        state)))


; while statement
; parameters: while condition, loop body, state
(define m_state_while
  (lambda (cond1 body state return)
    (if (m_boolean cond1 state)
        (m_state_while cond1 body (m_state body state) return)
 ;       (m_state (m_value body state) (m_state_while cond1 body (m_state body state)))
        state)))

; return statement
; paramteters: what you want to return
(define m_state_return
  (lambda (x)
    (if (state_member? x state)
        (m_statelookup x state) ;if it is a variable, return the variable
        (m_value_math(x))))) ;if it is an expression, return the value of the expression

; mstate declare assign is for the situation "var x = 1;". This will creates a variable and assigns it a value at the same time.
; parameters: the variable and the value
(define m_state_declare_assign
  (lambda (var_name value state)
    (state_add var_name (m_value value state) state)))

; mstate declare is for the situation "var x;". This  function creates a variable, but assigns it the value 'undef
; parameters: the variable
(define m_state_declare
  (lambda (var_name state)
    (state_add var_name 'undef state)))


; mstate assign is for the situation "x = 1;" This function removes the variable from the state and then adds it back
; to the state with the variable name and the value
; parameter: variable and the value
(define m_state_assign
  (lambda (var_name value state)
    (if (state_member? var_name state)
        (state_add var_name (m_value value state) (state_remove var_name state)) ;if the variable is in the state, declare the variable
        (error "Variable not declared"))))

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
      ((eq? op '<) <)
      ((eq? op '>) >)
      ((eq? op '<=) <=)
      ((eq? op '>=) >=)
      ((eq? op '==) =)
      ((eq? op '!=) !=)
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
        ((binary-ops (operator expr)) (m_value (operand1 expr) state) (m_value (operand2 expr) state))
        ((unary-ops (operator expr)) (m_value (operand1 expr) state)))))


; returns value of an assignment statement
(define m_value_statement
  (lambda (expr state)
    (if (or (eq? (stmt_type expr) '=) (eq? (stmt_type expr) 'var))
        (m_value (assigned_val expr) state)
        #f)))

; defines an operator that is a statement
(define statement '(var = if return))

; returns a value for part of the parse tree that is a list
(define m_value_list
  (lambda (expr state)
    (if (member? (stmt_type expr) statement)
        (m_value_statement expr state)
        (m_value_expression expr state))))

; changes #t and #f to true and false
(define return_helper
  (lambda (expr)
    (cond
      ((eq? expr #t) 'true)
      ((eq? expr #f) 'false)
      (else expr))))

; member? returns true or false depending on if x is in the lis
; parameters: atom to find, lis to look in
(define member?
  (lambda (x lis)
    (if (null? lis) #f
        (if (equal? x (car lis))
            #t
            (member? x (cdr lis))))))

; returns a value for part of the parse tree that is an atom
(define m_value_atom
  (lambda (expr state)
    (cond
      ((or (boolean? expr) (number? expr)) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      (else (state_lookup expr state)))))