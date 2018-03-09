; Giuliana Conte gdc24
; Ellie Fitzpatrick eef33
; Taylor Smith tps45
#lang racket
(require "simpleParser.scm") ; load parser
(require racket/trace)
; Takes a filename, calls parser with the filename, evaluates the parse tree returned by parser,
; and returns the proper value.
; Maintains a state for the variables and returns an error message if the user attempts to use a
; variable before it is declared. Uses the Scheme function (error ...) to return the error.

(define interpret_parsetree
  (lambda (parsetree state return)
    (if (null? parsetree)
        (if (eq? 'return (var1 (first_layer state)))
            (state_lookup 'return state)
            (error "no return statement."))
        (interpret_parsetree (cdr parsetree) (m_state (car parsetree) state return #f #f #f) return))))

(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (interpret_parsetree (parser filename) (state_new) return)))))

; defining commonly used words for abstraction
(define vars car)           ; list of variables in the state
(define vals cdr)           ; list of values in the state
(define var1 caar)          ; first variable in the state
(define val1 caadr)         ; first value in the state
(define empty_vars (list))  ; empty list of variables
(define empty_vals (list))  ; empty list of values
(define list_of_vals cadr)  ; list of the values
(define var_cdrs cdar)      ; all variables except the first
(define val_cdrs cdadr)     ; all values except the first

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; interacting with the state functions. implemented in two lists, now with layers!                                  ;
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

; removes the top layer from the state
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

; updates a variable's value without changing its layer
; parameters: a variable, its new value, and a state
(define state_update_val
  (lambda (var val state)
    (cond
      ((null? state) (error "No such variable."))
      ((eq? 'no_such_var (layer_lookup var (first_layer state))) ; variable not in layer
       (cons (first_layer state) (state_update_val var val (rest_of_layers state))))
      (else (cons  ; variable is in the layer
             (add_to_layer var val (remove_from_layer var (first_layer state)))
             (rest_of_layers state))))))

;(define state_remove
; (lambda (var state return)
;  (cond
;   ((null? (vars state)) return state)
;  ((eq? var (state_var1 state)) (state_cdrs state) return)
; (else (state_remove var (state_cdrs state) (lambda (v) (return (state_add (state_var1 state) (state val1 state) v))))))))

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
      ((null? state) (error "No such variable"))
      ((eq? 'no_such_var (layer_lookup var (first_layer state)))
       (state_lookup var (rest_of_layers state)))
      (else (layer_lookup var (first_layer state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; manipulating individual layers of the state                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(define remove_from_layer
  (lambda (var layer)
    (cond
      ((null? (vars layer)) layer) ; if it's null, just return the layer
      ((eq? var (var1 layer)) (layer_cdrs layer)) ; if it is the first variable, return the rest of the layer
      (else (add_to_layer (var1 layer) (val1 layer) (remove_from_layer var (layer_cdrs layer)))))))

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
; the main m_state, m_value, and m_boolean functions to return the values from the parse tree                       ;
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
  (lambda (expr state return break continue throw)
    (m_state_statement expr state return break continue throw)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; M_state functions                                                                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define stmt_type car) ; statement type, i.e. if, while, etc
(define empty_when_no_else cdddr) ; if the if statement has no else block, this will be an empty list
(define empty_when_only_assigning cddr) ; if the statement is only assigning and not declaring, this will be an empty list
(define declared_var cadr)
(define assigned_val caddr)
(define stmtlist cdr)
(define first_stmt car)
(define rest_of_stmts cdr)
(define cond1 cadr)
(define then-stmt caddr)
(define else-stmt cadddr)
(define return_val cadr)
(define try-body cadr)
(define catch-stmt caddr)
(define catch-block caddr)
(define finally-stmt cadddr)
(define finally-block cadr)
(define throw-val cadr)

; returns the updated state after the statement is evaluated
; parameters: a statement and a state
(define m_state_statement
  (lambda (stmt state return break continue throw)
    (cond
      ((eq? 'begin (stmt_type stmt)) (m_state_block (stmtlist stmt) state return break continue throw))
      ((and (eq? 'if (stmt_type stmt))
            (not (eq? (empty_when_no_else stmt) '())))
       (m_state_if_else (cond1 stmt) (then-stmt stmt) (else-stmt stmt) state return break continue throw))
      ((eq? 'if (stmt_type stmt)) (m_state_if (cond1 stmt) (then-stmt stmt) state return break continue throw))
      ((and (eq? 'var (stmt_type stmt))
            (not (eq? (empty_when_only_assigning stmt) '())))
       (m_state_declare_assign (declared_var stmt) (assigned_val stmt) state return break continue throw))
      ((eq? '= (stmt_type stmt)) (m_state_assign (declared_var stmt) (assigned_val stmt) state return break continue throw))
      ((eq? 'var (stmt_type stmt)) (m_state_declare (declared_var stmt) state return break continue throw))
      ((eq? 'return (stmt_type stmt)) (return (return_helper (m_value (declared_var stmt) state))))
      ((eq? 'break (stmt_type stmt)) (break (cons 'broken (remove_layer state))))
      ((eq? 'continue (stmt_type stmt)) (continue (cons 'conted (remove_layer state))))
      ((eq? 'try (stmt_type stmt)) (m_state_try (try-body stmt) (catch-stmt stmt) (finally-stmt stmt) state return break continue throw))
      ((eq? 'throw (stmt_type stmt)) (m_state_throw ....))
      ;((eq? 'return (stmt_type stmt)) (toAtoms (state_add 'return (return_helper (m_value (declared_var stmt) state)) state))); (state_remove 'return state))))
      ((eq? 'while (stmt_type stmt)) (m_state_while (cond1 stmt) (then-stmt stmt) state return break continue throw)))))

;(m_state_return stmt state break continue return))
  
; returns the updated state after executing a block of statements
; parameters: a block of code and a state
(define m_state_block
  (lambda (block state return break continue throw)
    (if (null? block) 
        state
        (remove_layer (m_state_stmtlist block (add_layer state) return break continue throw)))))

; returns the updated state after executing a list of statements
; parameters: a list of statements and a state
(define m_state_stmtlist
  (lambda (stmtlist state return break continue throw)
    (if (null? stmtlist)
        state
        (m_state_stmtlist (rest_of_stmts stmtlist) (m_state (first_stmt stmtlist) state break return continue throw) break continue return throw))))

; returns the updated state after executing an if/else statement
; parameters: condition, then statment, else statement, and state
(define m_state_if_else
  (lambda (cond1 then-stmt else-stmt state return break continue throw)
    (if (m_boolean cond1 state)
        (m_state then-stmt state return break continue throw)
        (m_state else-stmt state return break continue throw))))

; returns the updated state after executing an if statement without an else
; parameters: condition, then statment, and state
(define m_state_if
  (lambda (cond1 then-stmt state return break continue throw)
    (if (m_boolean cond1 state)
        (m_state then-stmt state return break continue throw)
        state)))

; returns the updated state after executing a while statement
; parameters: while condition, loop body, state, and return
(define m_state_while_helper
     (lambda (cond1 body state return break continue throw)
          (if (m_boolean cond1 state) 
<<<<<<< HEAD
              (m_state_while cond1 body (m_state body state return break continue) return break continue_new throw)
=======
              (m_state_while cond1 body (m_state body state return break continue) return break continue)
>>>>>>> 1ca32fda0850cfdee7f1b53be27239e72584454e
              state)))
      
(define m_state_while
  (lambda (cond1 body state return break continue throw)
    (let* ((computed (call/cc (lambda (break)
                       (call/cc (lambda (continue)
<<<<<<< HEAD
                                  (m_state_while cond1 body state return break continue throw)))))))
            (cond
              ((eq? 'conted (car computed)) (m_state_while cond1 (cadr computed) return break continue throw))
=======
                                  (m_state_while_helper cond1 body state return break continue)))))))
            (cond
              ((eq? 'conted (car computed)) (m_state_while cond1 body (cadr computed) return break continue))
>>>>>>> 1ca32fda0850cfdee7f1b53be27239e72584454e
              ((eq? 'broken (car computed)) (cadr computed))
              (else state)))))     


(trace m_state_while)
(trace m_state_statement)
(trace m_state_while_helper)
(trace m_state)
;(if (m_boolean cond1 state) 
 ;             (m_state_while cond1 body (m_state body state return break continue) return break continue_new)
  ;            state)))))))

; returns the program's return value
; paramteters: what you want to return
;(define m_state_return
;  (lambda (var)
;    (if (state_member? var state)
;        (m_statelookup var state) ;if it is a variable, return the variable
;        (m_value_math(var))))) ;if it is an expression, return the value of the expression
;state_update_val( var val state
;state var val
                      
(define m_state_return
  (lambda (expr state return break continue throw)
    (return (state_update_val 'return (m_value (cadr expr) state) (m_state (cadr expr) state return break continue) break continue throw))))
    

; returns the updated state after executing a declare & assign statement
; parameters: the variable and the value
(define m_state_declare_assign
  (lambda (var_name value state return break continue throw)
    (state_add var_name (m_value value state) state)))

; returns the updated state after executing a declare statement
; parameters: the variable
(define m_state_declare
  (lambda (var_name state return break continue throw)
    (state_add var_name 'undef state)))

; returns the updated state after executing an assignment statement
; parameter: variable and the value
(define m_state_assign
  (lambda (var_name value state return break continue throw)
    (if (state_member? var_name state)
        (state_update_val var_name (m_value value state) state)
        (error "Variable not declared"))))

; returns the updated state after executing a try/catch/finally block
; parameters: a try body, a catch statement, a finally statement, a state, return, break, continue and throw
(define m_state_try
  (lambda (try-body catch-stmt finally-stmt state return break continue throw)
    (m_state_finally finally-stmt (call/cc
                              (lambda (leave)
                                (leave (m_state_block
                                        try-body state return break continue (lambda (try-state exception)
                                                                               (leave (m_state_catch catch-stmt exception try-state return break throw))))))) return break continue throw)))

(define m_state_finally
  (lambda (finally-stmt state return break continue throw)
    (if (null? finally-stmt)
        state
        (m_state_block (finally-block finally-stmt) state return break continue throw))))


(define m_state_catch
  (lambda (catch-stmt exception state return break continue throw)
    (if (null? catch-stmt)
        state
        (m_state_block (catch-block catch-stmt) (add_to_state (exception-val catch-stmt) (m_value exception) (add_layer state)) return break continue throw))))

      

;      ((and (catch? catch-stmt)      ; there is both a catch and finally block
;            (finally? finally-stmt))
;       (something))
;       (m_state_block (finally-block finally-stmt)
;                      (remove_layer (m_state_try_catch try-body catch-stmt finally-stmt (add_layer state) return break continue))))
;      ((catch? catch-stmt) ; there is only a catch statement
;       (m_state_block (catch-block catch-stmt) (m_state_try_helper try-body state return break continue) return break continue))
;      ((finally? finally-stmt) ; there is only a finally statement
;       (m_state_block (finally-block finally-stmt) (m_state_try_helper try-body state return break continue) return break continue)))))


;(define m_state_finally
;  (lambda finally-stmt state return break continue
;    (call/cc
;     (lambda (finally)
;       (m_state_block finally-stmt state return break continue))) return break continue))

;(define m_state_catch
;  (lambda catch-stmt state return break continue
;    (call/cc
;     (lambda (catch)
;       (m_state_block catch-stmt state return break continue))) return break conintue))

;(define m_state_try
;  (lambda try-stmt state return break continue
;    (call/cc
;     (lambda (break)
;       (m_state_statement try-stmt state return break continue)))))


;(define m_state_throw
;  (lambda (catch-stmt throw-stmt state return break continue)
;    (m_state_catch catch-stmt (m_value (throw-val throw-stmt) state) state return break continue)))

;(lambda (t) (throw
;             (M_state_catch stmt
;                            (car t)
;                            (add_layer (remove_layer (cadr t)))
;                            return break continue)))


;(define m_state_try_catch
;  (lambda (try-body catch-stmt finally-stmt state return break continue)
;    (call/cc
;     (lambda (throw)
;       (m_state try-body state (lambda (try-body) (throw catch-stmt throw state return break continue)) break continue)))))


;(define m_state_try_helper
; (lambda (try-body state return break continue)
;   (call/cc
;    (lambda (throw)
;      (m_state_block try-body state return throw continue)))))
                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Math operations                                                                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; returns the correct Scheme version of the binary operators
; parameters: a math binary operator
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

; a not equal operator
; parameters: two values
(define !=
  (lambda (x y)
    (not (= x y))))

; returns the correct Scheme version of the unary operators
; parameters: a math unary operator
(define unary-ops
  (lambda (op)
    (cond
      ((eq? op '!) not)
      ((eq? op '-) (lambda (x) (- 0 x))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; M_value functions                                                                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define statement '(var = if return)) ; defines an operator that is a statement

; returns the value of an arithemtic expression whether the operator is unary or binary
; parameters: an expression and a state
(define m_value_expression
  (lambda (expr state)
    (if (eq? 3 (length expr))
        ((binary-ops (operator expr)) (m_value (operand1 expr) state) (m_value (operand2 expr) state))
        ((unary-ops (operator expr)) (m_value (operand1 expr) state)))))

; returns value of an assignment statement
; parameters: an expression and a state
(define m_value_statement
  (lambda (expr state)
    (if (or (eq? (stmt_type expr) '=) (eq? (stmt_type expr) 'var))
        (m_value (assigned_val expr) state)
        #f)))

; returns a value for part of the parse tree that is a list
; parameters: an expression and a state
(define m_value_list
  (lambda (expr state)
    (if (member? (stmt_type expr) statement)
        (m_value_statement expr state)
        (m_value_expression expr state))))

; returns a value for part of the parse tree that is an atom
; parameters: an atom and a state
(define m_value_atom
  (lambda (atom state)
    (cond
      ((or (boolean? atom) (number? atom)) atom)
      ((eq? atom 'true) #t)
      ((eq? atom 'false) #f)
      (else (state_lookup atom state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Other functions/helpers                                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; returns
; parameters
(define toAtoms
  (lambda (x)
    (cond
      ((eq? #t x) 'true)
      ((eq? #f x) 'false)
      (else x))))

; true iff the input is an atom
; parameters: an input x
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

; changes #t and #f to true and false
; parameters: an expression
(define return_helper
  (lambda (expr)
    (cond
      ((eq? expr #t) 'true)
      ((eq? expr #f) 'false)
      (else expr))))

; returns true or false depending on if x is in the lis
; parameters: atom to find, lis to look in
(define member?
  (lambda (x lis)
    (if (null? lis) #f
        (if (equal? x (car lis))
            #t
            (member? x (cdr lis))))))

; returns true iff a catch stmt exists
; parameters: a statement
(define catch?
  (lambda (stmt)
    (if (not (eq? (catch-block stmt) '()))
        #t
        #f)))

; returns true iff a finally stmt exists
; parameters: a statement
(define finally?
  (lambda (stmt)
    (if (not (eq? (finally-block stmt) '()))
        #t
        #f)))