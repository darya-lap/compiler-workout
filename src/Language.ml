(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
module MyUtils =
  struct

  let initList n ~f =
    let rec initList' i n f =
      if i >= n then []
      else (f i) :: (initList' (i+1) n f)
    in initList' 0 n f

  let bti   = function true -> 1 | _ -> 0
  let itb b = b <> 0
  let toFunc op =
    let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)

  end
(* Values *)
module Value =
  struct

    @type t = Int of int | String of string | Array of t list with show

    let to_int = function 
    | Int n -> n 
    | _ -> failwith "int value expected"

    let to_string = function 
    | String s -> s 
    | _ -> failwith "string value expected"

    let to_array = function
    | Array a -> a
    | _       -> failwith "array value expected"

    let of_int    n = Int    n
    let of_string s = String s
    let of_array  a = Array  a

    let update_string s i x = String.init      (String.length s) (fun j -> if j = i then x else s.[j])
    let update_array  a i x = MyUtils.initList (List.length a)   (fun j -> if j = i then x else List.nth a j)

  end
       
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> Value.t; l : string -> Value.t; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end

(* Builtins *)
module Builtin =
  struct

    let eval (st, i, o, _) args = function
    | "read"     -> (match i with z::i' -> (st, i', o, Some (Value.of_int z)) | _ -> failwith "Unexpected end of input")
    | "write"    -> (st, i, o @ [Value.to_int @@ List.hd args], None)
    | "$elem"    -> let [b; j] = args in
                    (st, i, o, let i = Value.to_int j in
                               Some (match b with
                                     | Value.String s -> Value.of_int @@ Char.code s.[i]
                                     | Value.Array  a -> List.nth a i
                               )
                    )         
    | "$length"  -> (st, i, o, Some (Value.of_int (match List.hd args with Value.Array a -> List.length a | Value.String s -> String.length s)))
    | "$array"   -> (st, i, o, Some (Value.of_array args))
    | "isArray"  -> let [a] = args in (st, i, o, Some (Value.of_int @@ match a with Value.Array  _ -> 1 | _ -> 0))
    | "isString" -> let [a] = args in (st, i, o, Some (Value.of_int @@ match a with Value.String _ -> 1 | _ -> 0))                     
       
  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant   *) | Const  of int
    (* array              *) | Array  of t list
    (* string             *) | String of string
    (* S-expressions      *) | Sexp   of string * t list
    (* variable           *) | Var    of string
    (* binary operator    *) | Binop  of string * t * t
    (* element extraction *) | Elem   of t * t
    (* length             *) | Length of t 
    (* function call      *) | Call   of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * Value.t option
                                                            
    (* Expression evaluator

          val eval : env -> config -> t -> int * config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns a pair: the return value for the call and the resulting configuration
    *)                                                       
    let rec eval env ((st, i, o, r) as conf) expr = match expr with
      | Const n  -> (st, i, o, Some (Value.of_int n))
      | Array l  -> let (st, i, o, v) = eval_list env conf l in env#definition env "$array" v (st, i, o, None)
      | String s -> (st, i, o, Some (Value.of_string s))
      | Var x    -> (st, i, o, Some (State.eval st x))
      | Binop (op, x, y) -> let ((st, i, o, Some a) as conf') = eval env conf x in
                              let (st, i, o, Some b) = eval env conf' y in
                              (st, i, o, Some (Value.of_int @@ MyUtils.toFunc op (Value.to_int a) (Value.to_int b)))
      | Elem (a, i)      -> let (st, i, o, v) = eval_list env conf [a; i] in env#definition env "$elem" v (st, i, o, None)
      | Length n         -> let (st, i, o, v) = eval_list env conf [n] in env#definition env "$length" v (st, i, o, None)
      | Call (fName, argsE) -> let (st', i', o', args) = List.fold_left (fun (st, i, o, args) e ->
                                                                          let ((st, i, o, Some r) as conf') = eval env (st, i, o, None) e in
                                                                          (st, i, o, args @ [r])
                                                                        ) (st, i, o, []) argsE in 
                               env#definition env fName args (st', i', o', None)
    and eval_list env conf xs =
      let vs, (st, i, o, _) =
        List.fold_left
          (fun (acc, conf) x ->
             let (_, _, _, Some v) as conf = eval env conf x in
             v::acc, conf
          )
          ([], conf)
          xs
      in
      (st, i, o, List.rev vs)
         
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string                                                                                                                  
    *)
    ostap (                                      
      parse:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);

      primary:
        b:base acss:(-"[" !(parse) -"]")* l:("." %"length")?
        {
          let elements = List.fold_left (fun a i -> Elem (a, i)) b acss in match l with
            | Some x -> Length elements
            | None   -> elements
        };
      base:
        n:DECIMAL {Const n}
      | s:STRING  {String (String.sub s 1 ((String.length s) - 2))}
      | c:CHAR    {Const (Char.code c)}
      | "[" es:!(Util.list0 parse) "]" { Array es }
      | x:IDENT opt:("(" args:!(Util.list0 parse) ")" {Call (x, args)} | empty {Var x}) {opt}
      | -"(" parse -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    type t =
    (* assignment                       *) | Assign of string * Expr.t list * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list
                                                                    
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)

    let update st x v is =
      let rec update a v = function
      | []    -> v           
      | i::tl ->
          let i = Value.to_int i in
          (match a with
           | Value.String s when tl = [] -> Value.String (Value.update_string s i (Char.chr @@ Value.to_int v))
           | Value.Array a               -> Value.Array  (Value.update_array  a i (update (List.nth a i) v tl))
          ) 
      in
      State.update x (match is with [] -> v | _ -> update (State.eval st x) v is) st
          
      let metaOp x y = match x with
      | Skip -> y
      | _ -> match y with
        | Skip -> x
        | _ -> Seq (x, y)
    let rec eval env ((st, i, o, r) as conf) k stmt = match stmt with
      | Assign (x, is, e)          -> let (st, i, o, is) = Expr.eval_list env conf is in
                                      let (st, i, o, Some r) = Expr.eval env (st, i, o, None) e in
                                      eval env (update st x r is, i, o, None) Skip k
      | Seq (stmt1, stmt2)         -> eval env conf (metaOp stmt2 k) stmt1
      | Skip                       -> (match k with
                                        | Skip -> conf
                                        | _    -> eval env conf Skip k)
      | If (e, thenStmt, elseStmt) -> let ((st, i, o, Some r) as conf') = Expr.eval env conf e in 
                                        if MyUtils.itb (Value.to_int r) then eval env conf' k thenStmt 
                                        else eval env conf' k elseStmt
      | While (e, wStmt)           -> let ((st, i, o, Some r) as conf') = Expr.eval env conf e in
                                        if MyUtils.itb (Value.to_int r) then eval env conf' (metaOp stmt k) wStmt
                                        else eval env conf' Skip k
      | Repeat (ruStmt, e)         -> eval env conf (metaOp (While (Expr.Binop ("==", e, Expr.Const 0), ruStmt)) k) ruStmt
      | Call (fName, argsE)        -> eval env (Expr.eval env conf (Expr.Call (fName, argsE))) Skip k
      | Return x                   -> (match x with
                                        | Some x -> Expr.eval env conf x
                                        | None   -> conf
                                      )
         
    (* Statement parser *)
    ostap (
      simple:
        x:IDENT is:(-"[" !(Expr.parse) -"]")* ":=" e:!(Expr.parse) {Assign (x, is, e)};
      ifStmt:
        "if" e:!(Expr.parse) "then" thenBody:parse
	elifBranches: (%"elif" elifE:!(Expr.parse) %"then" elifBody:!(parse))*
	elseBranch: (%"else" elseBody:!(parse))?
	"fi" {
	       let elseBranch' = match elseBranch with
	         | Some x -> x
                 | None   -> Skip in
	       let expandedElseBody = List.fold_right (fun (e', body') else' -> If (e', body', else')) elifBranches elseBranch' in
	       If (e, thenBody, expandedElseBody)  
	     };
      whileStmt:
        "while" e:!(Expr.parse) "do" body:parse "od" {While (e, body)};
      forStmt:
        "for" initStmt:stmt "," whileCond:!(Expr.parse) "," forStmt:stmt
        "do" body:parse "od" {Seq (initStmt, While (whileCond, Seq (body, forStmt)))};
      repeatUntilStmt:
	"repeat" body:parse "until" e:!(Expr.parse) {Repeat (body, e)};
      control:
          ifStmt
        | whileStmt
        | forStmt
	| repeatUntilStmt
	| "skip" {Skip};
      call:
          fName:IDENT "(" argsE:(!(Expr.parse))* ")" {Call (fName, argsE)};
      returnStmt:
          "return" e:!(Expr.parse)? { Return e };
      stmt:
          simple 
        | control
        | call
        | returnStmt;
      parse:
          stmt1:stmt ";" rest:parse {Seq (stmt1, rest)}
        | stmt
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      arg  : IDENT;
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
         locs:(%"local" !(Util.list arg))?
        "{" body:!(Stmt.parse) "}" {
        (name, (args, (match locs with None -> [] | Some l -> l), body))
      }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args ((st, i, o, r) as conf) =
           try
             let xs, locs, s      =  snd @@ M.find f m in
             let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
             let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
             (State.leave st'' st, i', o', r')
           with Not_found -> Builtin.eval conf args f
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
