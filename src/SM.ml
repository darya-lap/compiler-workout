open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP   of string
(* put a constant on the stack     *) | CONST   of int
(* put a string on the stack       *) | STRING  of string                      
(* load a variable to the stack    *) | LD      of string
(* store a variable from the stack *) | ST      of string
(* store in an array               *) | STA     of string * int
(* a label                         *) | LABEL   of string
(* unconditional jump              *) | JMP     of string
(* conditional jump                *) | CJMP    of string * string
(* begins procedure definition     *) | BEGIN   of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL    of string * int * bool
(* returns from a function         *) | RET     of bool with show
                                                   
(* The type for the stack machine program *)
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
*)
type config = (prg * State.t) list * Value.t list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let split n l =
  let rec unzip (taken, rest) = function
  | 0 -> (List.rev taken, rest)
  | n -> let h::tl = rest in unzip (h::taken, tl) (n-1)
  in
  unzip ([], l) n
        
  let rec eval env (controlSt, st, ((s, i, o) as config)) p =
    let instrEval instr = match instr with
      | BINOP op -> (match st with
        | (y::x::xs) -> (Value.of_int (MyUtils.toFunc op (Value.to_int x) (Value.to_int y))::xs, config) 
        | _ -> failwith "Not enough elements in stack")
      | CONST n  -> ((Value.of_int n)::st, config)
      | STRING str -> (Value.of_string str::st, config)
      | LD x       -> ((State.eval s x)::st, config)
      | ST x       -> (match st with
                        | z::tail -> (tail, ((State.update x z s), i, o))
                        | _       -> failwith "Not enough elements in stack")
      | STA (x, n) -> let ((value::ids), tail) = split (n + 1) st in
                      let s' = Stmt.update s x value ids in
                      (tail, (s', i, o))
      | BEGIN (_, params, locals) -> let fState = State.enter s (params @ locals) in
                                     let fState', st' = List.fold_left (fun (s, x::st) name -> (State.update name x s, st)) (fState, st) params in
                                     (st', (fState', i, o))
      | LABEL _ -> (st, config)
      in match p with
        | x::xs -> (match x with 
          | JMP l -> eval env (controlSt, st, config) (env#labeled l)
          | CJMP (znz, l) -> (match st with
                                | head::tail -> let v = (Value.to_int head) in 
                                                if (znz = "z" && v == 0) || (znz = "nz" && v <> 0) then eval env (controlSt, tail, config) (env#labeled l)
                                                else eval env (controlSt, tail, config) xs 
                                | _          -> failwith "CJMP failed with empty stack"
                             )
          | CALL (f, n, fl) -> if env#is_label f then eval env ((xs, s)::controlSt, st, config) (env#labeled f)
                               else eval env (env#builtin (controlSt, st, config) f n (not fl)) xs
          | RET _ | END -> (match controlSt with
                              | (p, oldS)::controlSt' -> let s' = State.leave s oldS in
                                                         eval env (controlSt', st, (s', i, o)) p
                              | _                     -> (controlSt, st, config)
                           )
          | _ -> let (st, config) = instrEval x in
                 eval env (controlSt, st, config) xs)
        | _ -> (controlSt, st, config)

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
  let (_, _, (_, _, o)) =
    eval
      (object
         method is_label l = M.mem l m
         method labeled l = M.find l m
         method builtin (cstack, stack, (st, i, o)) f n p =
           let f = match f.[0] with 'L' -> String.sub f 1 (String.length f - 1) | _ -> f in
           let args, stack' = split n stack in
           let (st, i, o, r) = Language.Builtin.eval (st, i, o, None) args f in
           let stack'' = if p then stack' else let Some r = r in r::stack' in
           Printf.printf "Builtin: %s\n";
           (cstack, stack'', (st, i, o))
       end
      )
      ([], [], (State.empty, i, []))
      p
  in
  o

(* Stack machine compiler

     val compile : Language.t -> prg

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
  | Expr.String s         -> [STRING s]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (fName, argsE) -> let compiledArgs = List.flatten (List.map (expr) (List.rev argsE)) in
                                compiledArgs @ [CALL (fName, List.length argsE, true)]
  | Expr.Array xs         -> let compiledXs = List.flatten (List.map (expr) (List.rev xs)) in
                             compiledXs @ [CALL ("$array", (List.length compiledXs), true)]
  | Expr.Elem (a, i)      -> expr i @ expr a @ [CALL ("$elem", 2, true)]
  | Expr.Length n         -> expr n @ [CALL ("$length", 1, true)]
  in match p with
  | Stmt.Seq (s1, s2)  -> (let newLabel = labelGen#get in
                           let (compiled1, used1) = compileWithLabels s1 newLabel in
                           let (compiled2, used2) = compileWithLabels s2 lastL in
                           (compiled1 @ (if used1 then [LABEL newLabel] else []) @ compiled2), used2)
  | Stmt.Assign (x, is, e) -> (match is with
                                 | [] -> (expr e @ [ST x]), false
                                 | _  -> let compiledIs = List.fold_left (fun p id -> p @ (expr id)) [] (List.rev is) in
                                         compiledIs @ expr e @ [STA (x, List.length is)], false
                              )
  | Stmt.Assign (x, [], e) -> (expr e @ [ST x]), false
  | Stmt.Assign (x, is, e) -> let compiledIs = (List.fold_left (fun rest e -> expr e @ rest) [] (is @ [e])) in
                              (compiledIs @ [STA(x, List.length is)]), false
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
  | Stmt.Repeat (body, e) ->
    let lLoop = labelGen#get in
    let (repeatBody, _) = compileWithLabels body lastL in
    ([LABEL lLoop] @ repeatBody @ expr e @ [CJMP ("z", lLoop)]), false
  | Stmt.Skip -> [], false
  | Stmt.Call (fName, argsE) -> let compiledArgs = List.flatten (List.map (expr) (List.rev argsE)) in
                                compiledArgs @ [CALL (fName, List.length argsE, false)], false
  | Stmt.Return e -> (match e with
                       | Some x -> (expr x) @ [RET true]
                       | None   -> [RET false]), false

let compileP p =
  let label = labelGen#get in
  let compiled, used = compileWithLabels p label in
  compiled @ (if used then [LABEL label] else [])

let compileDefs defs =
  let compileDef (name, (params, locals, body)) = 
    (let compiledBody = compileP body in
    [LABEL name; BEGIN (name, params, locals)] @ compiledBody @ [END]) in
  List.flatten (List.map compileDef defs)

let rec compile (defs, main) =
  let compiledMain = compileP main in
  let compiledDefs = compileDefs defs in
  compiledMain @ [END] @ compiledDefs
