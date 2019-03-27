(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
   *)

let counter = ref 0;;

let next_var() = let result = "__repeat_variable__" ^ string_of_int !counter in
                counter := !counter + 1;
                result;;

open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
       
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

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
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
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | RepeatUntil of t * Expr.t  with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval c op =
      let (st, input, output) = c in
      match op with
         | Read var	-> (match input with
                 | x::rest -> (Expr.update var x st), rest, output
                 | [] -> failwith("No more input")
               )
         | Assign (var, exp) -> (Expr.update var (Expr.eval st exp) st), input, output
         | Seq (f, s)       -> eval (eval c f) s
         | Write exp      -> st, input, (output @ [Expr.eval st exp])
         | Skip                              -> (st, input, output)
         | If (e, thenStmt, elseStmt)        -> eval (st, input, output) (if Expr.int_to_bool (Expr.eval st e) then thenStmt else elseStmt)
         | While (e, wStmt)                  -> if Expr.int_to_bool (Expr.eval st e) then eval (eval (st, input, output) wStmt) op else (st, input, output)
         | RepeatUntil (ruStmt, e)           -> let (sNew, iNew, oNew) = eval (st, input, output) ruStmt in
                                                if not (Expr.int_to_bool (Expr.eval sNew e)) then eval (sNew, iNew, oNew) op else (sNew, iNew, oNew)
     (* Statement parser *)
     ostap (
      simple:
            "read" "(" x:IDENT ")"         {Read x}
          | "write" "(" e:!(Expr.expr) ")" {Write e}
          | x:IDENT ":=" e:!(Expr.expr)    {Assign (x, e)};
      ifStmt:
          "if" e:!(Expr.expr) "then" thenBody:parse
      elifBranches: (%"elif" elifE:!(Expr.expr) %"then" elifBody:!(parse))*
      elseBranch: (%"else" elseBody:!(parse))?
      "fi" {
            let elseBranch' = match elseBranch with
              | Some x -> x
                  | None   -> Skip in
            let expandedElseBody = List.fold_right (fun (e', body') else' -> If (e', body', else')) elifBranches elseBranch' in
            If (e, thenBody, expandedElseBody)
          };
      whileStmt:
          "while" e:!(Expr.expr) "do" body:parse "od" {While (e, body)};
      forStmt:
          "for" initStmt:stmt "," whileCond:!(Expr.expr) "," forStmt:stmt
          "do" body:parse "od" {Seq (initStmt, While (whileCond, Seq (body, forStmt)))};
      repeatUntilStmt:
          "repeat" body:parse "until" e:!(Expr.expr) {RepeatUntil (body, e)};
      control:
            ifStmt
          | whileStmt
          | forStmt
          | repeatUntilStmt
          | "skip" {Skip};
      stmt:
            simple
          | control;
      parse:
            stmt1:stmt ";" rest:parse {Seq (stmt1, rest)}
          | stmt
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

(* Top-level parser *)
let parse = Stmt.parse                                                     
