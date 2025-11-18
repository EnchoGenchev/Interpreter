(* Encho Genchev 113570835 *)

(* value type representing all stack values *)
type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Name of string
  | Error
  | Unit
  | Closure of env * string * string list (* closure: env snapshot, formal param, body code *)

and env = (string * value) list list (*each frame is a scope*)

(* string representation for output/println *)
let value_to_string = function
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool true -> ":true:"
  | Bool false -> ":false:"
  | Str s -> s
  | Name n -> n
  | Error -> ":error:"
  | Unit -> ":unit:"
  | Closure _ -> ":closure:"  (* internal only; closures don't print *)

(* strip surrounding quotes from a string *)
let strip_quotes str =
  let len = String.length str in
  if len >= 2 && str.[0] = '"' && str.[len - 1] = '"' then
    String.sub str 1 (len - 2)
  else str

(* check if a string is a valid int, return option *)
let parse_int_opt str =
  try Some (int_of_string str) with _ -> None

(* check if a string is a valid float, return option *)
let parse_float_opt str =
  try Some (float_of_string str) with _ -> None

let is_valid_name str =
  let len = String.length str in
  if len = 0 then false
  else
    let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') in
    let is_digit c = '0' <= c && c <= '9' in
    let is_underscore c = c = '_' in
    (is_letter str.[0] || is_underscore str.[0]) &&
    String.for_all (fun c -> is_letter c || is_digit c || is_underscore c) str

let parse str =
  match str with
  | ":true:" -> Some (Bool true)
  | ":false:" -> Some (Bool false)
  | ":error:" -> Some Error
  | ":unit:" -> Some Unit
  | _ ->
    if String.length str >= 2 && str.[0] = '"' && str.[String.length str - 1] = '"' then
      Some (Str (strip_quotes str))
    else
      match parse_int_opt str, parse_float_opt str with
      | Some n, _ -> Some (Int n)
      | None, Some f -> Some (Float f)
      | _ -> if is_valid_name str then Some (Name str) else None

let push stack str =
  match parse str with
  | Some value -> value :: stack
  | None -> Error :: stack

let pop stack =
  match stack with
  | [] -> [Error]
  | _::tl -> tl

let rec env_lookup name env =
  match env with
  | [] -> None
  | frame :: rest ->
    (match List.assoc_opt name frame with
    | Some v -> Some v
    | None -> env_lookup name rest)

let env_add name value env =
  match env with
  | frame :: rest -> ((name, value)::frame) :: rest
  | [] -> [[(name, value)]]

let env_update name value env =
  let rec env_helper = function
    | [] -> []
    | frame :: rest ->
      if List.exists (fun (n,_) -> n = name) frame
      then (List.map (fun (n,v) -> if n = name then (n,value) else (n,v)) frame)::rest
      else frame :: env_helper rest
  in
  env_helper env

let rec resolve value env =
  match value with
  | Name n -> (match env_lookup n env with Some v -> v | None -> Error)
  | v -> v

(* Arithmetic helpers with int and float support *)
let add_vals v1 v2 =
  match v1, v2 with
  | Int x, Int y -> Int (x + y)
  | Float x, Float y -> Float (x +. y)
  | Int x, Float y -> Float (float_of_int x +. y)
  | Float x, Int y -> Float (x +. float_of_int y)
  | _, _ -> Error

let sub_vals v1 v2 =
  match v1, v2 with
  | Int x, Int y -> Int (x - y)
  | Float x, Float y -> Float (x -. y)
  | Int x, Float y -> Float (float_of_int x -. y)
  | Float x, Int y -> Float (x -. float_of_int y)
  | _, _ -> Error

let mult_vals v1 v2 =
  match v1, v2 with
  | Int x, Int y -> Int (x * y)
  | Float x, Float y -> Float (x *. y)
  | Int x, Float y -> Float (float_of_int x *. y)
  | Float x, Int y -> Float (x *. float_of_int y)
  | _, _ -> Error

let div_vals v1 v2 =
  match v1, v2 with
  | Int _, Int 0 | Float _, Float 0.0 -> Error
  | Int x, Int y -> Int (x / y)
  | Float x, Float y -> Float (x /. y)
  | Int x, Float y -> if y = 0.0 then Error else Float (float_of_int x /. y)
  | Float x, Int y -> if y = 0 then Error else Float (x /. float_of_int y)
  | _, _ -> Error

let rem_vals v1 v2 =
  match v1, v2 with
  | Int _, Int 0 -> Error
  | Int x, Int y -> Int (x mod y)
  | _, _ -> Error

let sign_val v =
  match v with
  | Int x -> Int (if x = 0 then 0 else -x)
  | Float f -> Float (if f = 0. then 0. else -. f)
  | _ -> Error

let add stack env =
  match stack with
  | v2 :: v1 :: tl ->
    let rv1 = resolve v1 env in
    let rv2 = resolve v2 env in
    (match add_vals rv1 rv2 with
     | Error -> v2 :: v1 :: Error :: tl
     | res -> res :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let sub stack env =
  match stack with
  | v2 :: v1 :: tl ->
    let rv1 = resolve v1 env in
    let rv2 = resolve v2 env in
    (match sub_vals rv1 rv2 with
     | Error -> v2 :: v1 :: Error :: tl
     | res -> res :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let mult stack env =
  match stack with
  | v2 :: v1 :: tl ->
    let rv1 = resolve v1 env in
    let rv2 = resolve v2 env in
    (match mult_vals rv1 rv2 with
     | Error -> v2 :: v1 :: Error :: tl
     | res -> res :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let div stack env =
  match stack with
  | v2 :: v1 :: tl ->
    let rv1 = resolve v1 env in
    let rv2 = resolve v2 env in
    (match div_vals rv1 rv2 with
     | Error -> Error :: v2 :: v1 :: tl
     | res -> res :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let rem stack env =
  match stack with
  | v2 :: v1 :: tl ->
    let rv1 = resolve v1 env in
    let rv2 = resolve v2 env in
    (match rem_vals rv1 rv2 with
     | Error -> v2 :: v1 :: Error :: tl
     | res -> res :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let sign stack env =
  match stack with
  | v :: tl ->
    let rv = resolve v env in
    (match sign_val rv with
     | Error -> v :: Error :: tl
     | res -> res :: tl)
  | [] -> [Error]

let swap stack =
  match stack with
  | v1 :: v2 :: tl -> v2 :: v1 :: tl
  | [v] -> v :: Error :: []
  | [] -> [Error]

let to_string_stack stack =
  match stack with
  | value :: tl -> Str (value_to_string value) :: tl
  | [] -> [Error]

let println stack out_file =
  match stack with
  | value :: tl -> output_string out_file (value_to_string value ^ "\n"); tl
  | [] -> [Error]

let cat stack env =
  match stack with
  | Str y :: Str x :: tl -> Str (x ^ y) :: tl
  | v1 :: v2 :: tl -> v2 :: v1 :: Error :: tl
  | [v] -> v :: Error :: []
  | [] -> [Error]

let and_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
     | Bool x, Bool y -> Bool (x && y) :: tl
     | _ -> Error :: v2 :: v1 :: tl)
  | [v] -> Error :: v :: []
  | [] -> [Error]

let or_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
     | Bool x, Bool y -> Bool (x || y) :: tl
     | _ -> Error :: v2 :: v1 :: tl)
  | [v] -> Error :: v :: []
  | [] -> [Error]

let not_op stack env =
  match stack with
  | v :: tl ->
    (match resolve v env with
     | Bool x -> Bool (not x) :: tl
     | _ -> Error :: v :: tl)
  | [] -> [Error]

let equal_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
     | Int x, Int y -> Bool (x = y) :: tl
     | Float x, Float y -> Bool (x = y) :: tl
     | Int x, Float y -> Bool ((float_of_int x) = y) :: tl
     | Float x, Int y -> Bool (x = float_of_int y) :: tl
     | _ -> Error :: v2 :: v1 :: tl)
  | [v] -> Error :: v :: []
  | [] -> [Error]

let less_than_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
     | Int x, Int y -> Bool (x < y) :: tl
     | Float x, Float y -> Bool (x < y) :: tl
     | Int x, Float y -> Bool ((float_of_int x) < y) :: tl
     | Float x, Int y -> Bool (x < float_of_int y) :: tl
     | _ -> Error :: v2 :: v1 :: tl)
  | [v] -> Error :: v :: []
  | [] -> [Error]

let assign stack env =
  match stack with
  | v :: Name n :: tl ->
    let resolve_v =
      match v with
      | Name name ->
        (match env_lookup name env with
        | Some value -> if value = Error then None else Some value
        | None -> None)
      | Error -> None
      | _ -> Some v
    in
    (match resolve_v with
     | Some value -> Unit :: tl, env_add n value env
     | None -> Error :: v :: Name n :: tl, env)
  | v :: n :: tl -> Error :: v :: n :: tl, env
  | [v] -> Error :: v :: [], env
  | [] -> [Error], env

let if_op stack env =
  match stack with
  | x :: y :: z :: tl -> (
    match x, y, resolve z env with
    | vx, vy, Bool cond -> if cond then vx :: tl, env else vy :: tl, env
    | _, _, _ -> Error :: x :: y :: z :: tl, env)
  | [a; b] -> Error :: a :: b :: [], env
  | [a] -> Error :: a :: [], env
  | [] -> [Error], env

(* interpret a single command, returns updated stack, stack_env, env, and remaining cmds *)
let rec interpret_command
  (stack : value list)
  (stack_env : value list list)
  (env : env)
  (cmd : string)
  (out_file : out_channel)
  : value list * value list list * env * string list =
  let trimmed = String.trim cmd in
  if String.length trimmed = 0 then (stack, stack_env, env, [])
  else
    let words = String.split_on_char ' ' trimmed in
    match words with
    (* function declaration *)
    | "fun" :: fname :: param :: rest ->
        (* gather function body until funEnd *)
        let rec gather_fun_body acc remaining =
          match remaining with
          | [] -> (List.rev acc, [])
          | line :: tl ->
            if String.trim line = "funEnd" then (List.rev acc, tl)
            else gather_fun_body (line :: acc) tl
        in
        let (body, remaining) = gather_fun_body [] rest in
        let closure = Closure (env, param, body) in
        let new_env = env_add fname closure env in
        (Unit :: stack, stack_env, new_env, remaining)

    (* function call *)
    | ["call"] -> (
      match stack with
      | arg :: Name fname :: tl -> (
        match env_lookup fname env with
        | Some (Closure (closure_env, param, body)) ->
          let arg_val = resolve arg env in
          if arg_val = Error then (Error :: stack, stack_env, env, [])
          else
            let new_env = env_add param arg_val closure_env in
            let func_stack = [] in
            (* execute function body *)
            let rec process_fun stk stk_env envs cmds =
              match cmds with
              | [] -> stk
              | cmd_hd :: cmd_tl ->
                if cmd_hd = "return" then stk
                else
                  let s, se, e, rem = interpret_command stk stk_env envs cmd_hd out_file in
                  process_fun s se e cmd_tl
            in
            let result_stack = process_fun func_stack [] new_env body in
            (match result_stack with
            | v :: _ -> (v :: tl, stack_env, env, [])
            | [] -> (Error :: tl, stack_env, env, []))
        | _ -> (Error :: stack, stack_env, env, []))
      | _ -> (Error :: stack, stack_env, env, []))

    (* basic commands *)
    | ["push"; arg] -> (push stack arg, stack_env, env, [])
    | "push" :: rest ->
        let arg = String.concat " " rest in
        (push stack arg, stack_env, env, [])
    | ["pop"] -> (pop stack, stack_env, env, [])
    | ["add"] -> (add stack env, stack_env, env, [])
    | ["sub"] -> (sub stack env, stack_env, env, [])
    | ["mult"] -> (mult stack env, stack_env, env, [])
    | ["div"] -> (div stack env, stack_env, env, [])
    | ["rem"] -> (rem stack env, stack_env, env, [])
    | ["sign"] -> (sign stack env, stack_env, env, [])
    | ["swap"] -> (swap stack, stack_env, env, [])
    | ["toString"] -> (to_string_stack stack, stack_env, env, [])
    | ["println"] -> (println stack out_file, stack_env, env, [])
    | ["cat"] -> (cat stack env, stack_env, env, [])
    | ["and"] -> (and_op stack env, stack_env, env, [])
    | ["or"] -> (or_op stack env, stack_env, env, [])
    | ["not"] -> (not_op stack env, stack_env, env, [])
    | ["equal"] -> (equal_op stack env, stack_env, env, [])
    | ["lessThan"] -> (less_than_op stack env, stack_env, env, [])
    | ["assign"] -> let s,e = assign stack env in (s, stack_env, e, [])
    | ["if"] -> let s,e = if_op stack env in (s, stack_env, e, [])
    (* environment handling *)
    | ["let"] -> ([], stack :: stack_env, [] :: env, [])
    | ["end"] -> (
      match stack_env with
      | outer_stack :: rest_stack_env ->
        let top =
          match stack with
          | v :: _ -> v
          | [] -> Error
        in
        (top :: outer_stack, rest_stack_env, (match env with _ :: rest_env -> rest_env | [] -> []), [])
      | [] -> (Error :: stack, [], env, []))
    | _ -> (stack, stack_env, env, [])

(* main loop processing all commands *)
let rec process
    (stack : value list)
    (stack_env : value list list)
    (env : env)
    (commands : string list)
    (out_file : out_channel)
    : value list =
  match commands with
  | [] -> stack
  | cmd :: rest ->
    if String.trim cmd = "quit" then stack
    else
      let updated_stack, updated_stack_env, updated_env, remaining =
        interpret_command stack stack_env env cmd out_file
      in
      let next_cmds = if remaining = [] then rest else remaining @ rest in
      process updated_stack updated_stack_env updated_env next_cmds out_file

(* main function *)
let interpreter ((input : string), (output : string)) : unit =
  let in_file = open_in input in
  let out_file = open_out output in

  let rec read_lines acc =
    try read_lines (input_line in_file :: acc)
    with End_of_file -> List.rev acc
  in
  let commands = read_lines [] in

  ignore (process [] [] [[]] commands out_file);
  close_in in_file;
  close_out out_file

;;

(* interpreter invocation example *)
interpreter ("input5.txt", "output5.txt");
