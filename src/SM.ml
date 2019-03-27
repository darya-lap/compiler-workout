open List
open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let instrEval (stack, (s, inp, out)) instruction = match instruction with
  | READ     -> (match inp with
    | z :: tail -> (z :: stack, (s, tail, out))
    | _         -> failwith "Not enough elements in input")
  | WRITE    -> (match stack with
    | z :: tail -> (tail, (s, inp, out @ [z]))
    | _         -> failwith "Not enough elements in stack")
  | BINOP op -> (match stack with
    | y :: x :: tail -> ((Language.Expr.operator op x y) :: tail, (s, inp, out))
    | _              -> failwith "Not enough elements in stack")
  | CONST z  -> (z :: stack, (s, inp, out))
  | ST x     -> (match stack with
    | z :: tail -> (tail, (Language.Expr.update x z s, inp, out))
    | _         -> failwith "Not enough elements in stack")
  | LABEL l  ->  (stack, (s, inp, out))
  | LD x     -> ((s x) :: stack, (s, inp, out))

 let rec eval env cfg p = match p with
  | instr::tail -> (match instr with
    | LABEL l        -> eval env cfg tail
    | JMP l          -> eval env cfg (env#labeled l)
    | CJMP (znz, l)  -> (let (st, rem) = cfg in match znz with
                          | "z"  -> (match st with
                                    | z::st' -> if z <> 0 then (eval env (st', rem) tail) else (eval env (st', rem) (env#labeled l))
                                    | []     -> failwith "CJMP with empty stack")
                          | "nz" -> (match st with
                                    | z::st' -> if z <> 0 then (eval env (st', rem) (env#labeled l)) else (eval env (st', rem) tail)
                                    | []     -> failwith "CJMP with empty stack"))
    | _              -> eval env (instrEval cfg instr) tail)
  | []          -> cfg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let labelGen = object
   val mutable freeLabel = 0
   method get = freeLabel <- freeLabel + 1; "L" ^ string_of_int freeLabel
end

 let rec compileWithLabels p lastL =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in match p with
  | Stmt.Seq (s1, s2)  -> (let newLabel = labelGen#get in
                           let (compiled1, used1) = compileWithLabels s1 newLabel in
                           let (compiled2, used2) = compileWithLabels s2 lastL in
                           (compiled1 @ (if used1 then [LABEL newLabel] else []) @ compiled2), used2)
  | Stmt.Read x        -> [READ; ST x], false
  | Stmt.Write e       -> (expr e @ [WRITE]), false
  | Stmt.Assign (x, e) -> (expr e @ [ST x]), false
  | Stmt.If (e, s1, s2) ->
    let lElse = labelGen#get in
    let (compiledS1, used1) = compileWithLabels s1 lastL in
    let (compiledS2, used2) = compileWithLabels s2 lastL in
    (expr e @ [CJMP ("z", lElse)]
    @ compiledS1 @ (if used1 then [] else [JMP lastL]) @ [LABEL lElse]
    @ compiledS2 @ (if used2 then [] else [JMP lastL])), true
  | Stmt.While (e, body) ->
    let lCheck = labelGen#get in
    let lLoop = labelGen#get in
    let (doBody, _) = compileWithLabels body lCheck in
    ([JMP lCheck; LABEL lLoop] @ doBody @ [LABEL lCheck] @ expr e @ [CJMP ("nz", lLoop)]), false
  | Stmt.RepeatUntil (body, e) ->
    let lLoop = labelGen#get in
    let (repeatBody, _) = compileWithLabels body lastL in
    ([LABEL lLoop] @ repeatBody @ expr e @ [CJMP ("z", lLoop)]), false
  | Stmt.Skip -> [], false

 let rec compile p =
  let label = labelGen#get in
  let compiled, used = compileWithLabels p label in
  compiled @ (if used then [LABEL label] else []) 
