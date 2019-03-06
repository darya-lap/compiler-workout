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
 let interpreter sm evt =
	let (stack, f) = sm in
	let (st, input, output) = f in
	match evt with
	| LD var   -> [st var] @ stack, f
	| ST var   -> (match stack with
		              | x::rest -> rest, (Language.Expr.update var x st, input, output)
                )
	| READ     -> (match input with
		              | x::rest -> [x] @ stack, (st, rest, output)
				)
	| WRITE    -> (match stack with
		              | x::rest -> rest, (st, input, output @ [x])
                )
	| BINOP op -> (match stack with
	| y::x::rest -> [Language.Expr.operator op x y] @ rest, f
			)
	| CONST x  -> [x] @ stack, f
let eval sm prog = List.fold_left interpreter sm prog

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

  (* Expression needs in their own compiler *)
  let rec compileE e = match e with
      | Language.Expr.Const n -> [CONST n]
      | Language.Expr.Var x -> [LD x]
      | Language.Expr.Binop (operator, left, right) -> (compileE left) @ (compileE right) @ [BINOP operator];;

  let rec compile program = match program with
      | Language.Stmt.Assign (x, e) -> (compileE e) @ [ST x]
      | Language.Stmt.Read x -> [READ; ST x]
      | Language.Stmt.Write e -> (compileE e) @ [WRITE]
      | Language.Stmt.Seq (a, b) -> (compile a) @ (compile b);;