(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let bool_to_int b = if b then 1 else 0 
    let int_to_bool i = i != 0 

    let operator s =
        match s with
        | "+"  -> ( + )
        | "-"  -> ( - )
        | "*"  -> ( * )
        | "/"  -> ( / )
        | "%"  -> ( mod )
        | "<"  -> fun left right -> bool_to_int (( < ) left right)
        | ">"  -> fun left right -> bool_to_int (( > ) left right)
        | "<=" -> fun left right -> bool_to_int (( <= ) left right)
        | ">=" -> fun left right -> bool_to_int (( >= ) left right)
        | "==" -> fun left right -> bool_to_int (( == ) left right)
        | "!=" -> fun left right -> bool_to_int (( != ) left right)
        | "&&" -> fun left right -> bool_to_int (( && ) (int_to_bool left) (int_to_bool right))
        | "!!" -> fun left right -> bool_to_int (( || ) (int_to_bool left) (int_to_bool right))

    let rec eval st exp =
        match exp with
        | Const value -> value
        | Var name   -> st name
        | Binop (op, left, right) -> (operator op) (eval st left) (eval st right);;

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)

    let parseBinOp op = ostap(- $(op)), (fun x y -> Binop (op, x, y))

    (* Statement parser *)
    ostap (
      expr:
          !(Ostap.Util.expr
            (fun x -> x)  (* identity function *)
            (Array.map (fun (asc, ops) -> asc, List.map parseBinOp ops)
                  [|
                      `Lefta, ["!!"];
                      `Lefta, ["&&"];
                      `Nona , ["=="; "!="];
                      `Nona , ["<="; "<"; ">="; ">"];
                      `Lefta, ["+"; "-"];
                      `Lefta, ["*"; "/"; "%"];
                  |]
              )
            primary
        );
      primary: x:IDENT {Var x} | c:DECIMAL {Const c} | -"(" expr -")"  (* simpiest expression - {var, const, (var), (const)}*)
    )
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval conf op =
      let (st, input, output) = conf in
      match op with
        | Read var	-> (match input with
                | x::rest -> (Expr.update var x st), rest, output
                | [] -> failwith("No more input")
              )
        | Write exp      -> st, input, (output @ [Expr.eval st exp])
        | Seq (first, second)       -> eval (eval conf first) second
        | Assign (var, exp) -> (Expr.update var (Expr.eval st exp) st), input, output  

    (* Statement parser *)
    ostap (
      unitStmt:
  			  "read" "(" x:IDENT ")" {Read x}
  			| "write" "(" e:!(Expr.expr) ")" {Write e}
        | x:IDENT ":=" e:!(Expr.expr) {Assign(x, e)};

      parse:
          s:unitStmt ";" rest:parse {Seq(s, rest)}
        | unitStmt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
