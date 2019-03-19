open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
*)                         
let eval_insn config insn = 
  let (stack, stmt_config) = config in
  let (state, input, output) = stmt_config in
  match insn with
  | BINOP operator -> (match stack with
    | y::x::tail -> ([(Language.Expr.eval_op operator) x y]@tail, stmt_config))
    | CONST value -> ([value]@stack, stmt_config)                 
  | READ -> (match input with
    | head::tail -> ([head]@stack, (state, tail, output)))
  | WRITE -> (match stack with
    | head::tail -> (tail, (state, input, output@[head])))
  | LD  variable_name -> ([state variable_name]@stack, stmt_config)
  | ST  variable_name -> (match stack with
    | head::tail -> (tail, (Language.Expr.update variable_name head state, input, output)))

let rec eval config insn : config = List.fold_left eval_insn config insn

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
