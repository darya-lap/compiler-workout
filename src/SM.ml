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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env (cst, st, (s, input, output)) prg =
  match prg with
  | []            -> (cst, st, (s,  input, output))
  | BINOP op :: p ->
      let y :: x :: st1 = st in
      let res = Expr.eval s (Binop (op, Const x, Const y))
      in eval env (cst, res :: st1, (s,  input, output)) p
  | CONST c  :: p -> eval env (cst, c :: st, (s, input, output)) p
  | READ     :: p -> eval env (cst, (List.hd input) :: st, (s, List.tl  input, output)) p
  | WRITE    :: p -> eval env (cst, List.tl st, (s,  input, output @ [List.hd st])) p
  | LD x     :: p -> eval env (cst, Language.State.eval s x :: st, (s, input, output)) p
  | ST x     :: p -> eval env (cst, List.tl st, (Language.State.update x (List.hd st) s,  input, output)) p
  | LABEL l  :: p -> eval env (cst, st, (s, input, output)) p
  | JMP l    :: _ -> eval env (cst, st, (s, input, output)) (env#labeled l)
  | CJMP (z, l) :: p ->
      let b = if z = "z" then (List.hd st) == 0 else (List.hd st) != 0 in
      if b then eval env (cst, List.tl st, (s, input, output)) (env#labeled l) else eval env (cst, List.tl st, (s, input, output)) p
  | BEGIN (a, l) :: p ->
      let state = Language.State.push_scope s (a @ l) in
      let s, st = List.fold_left (fun (s, x::stack) name -> (State.update name x s, stack)) (state, st) a in
      eval env (cst, st, (s, input, output)) p
  | CALL f   :: p -> eval env ((p, s)::cst, st, (s, input, output)) (env#labeled f)
  | END      :: _ -> match cst with
      | (p, old_s)::cst -> eval env (cst, st, (Language.State.drop_scope s old_s, input, output)) p
      | _ -> (cst, st, (s, input, output))

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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let label = object
    val mutable n = 0
    method get s = n <- n + 1; s ^ string_of_int n
end
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
  | Stmt.Repeat  (body, e) ->
    let lLoop = labelGen#get in
    let (repeatBody, _) = compileWithLabels body lastL in
    ([LABEL lLoop] @ repeatBody @ expr e @ [CJMP ("z", lLoop)]), false
  | Stmt.Skip -> [], false
  | Stmt.Call (f, args) ->
    List.concat (List.map (expr) (List.rev args)) @ [CALL f], false

let rec compile_main p =
    let l = label#get "l_main" in
    let compiled, used = compileWithLabels p l in
    compiled @ (if used then [LABEL l] else [])

let rec compile_defs defs =
List.fold_left (fun (p) (name, (args, locals, body)) ->
    let body = compile_main body in
    p @ [LABEL name] @ [BEGIN (args, locals)] @ body @ [END]) ([]) defs

let rec compile (defs, main) =
let main = compile_main main in
let defs = compile_defs defs in
main @ [END] @ defs
