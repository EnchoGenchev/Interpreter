(*Encho Genchev 113570835*)

(*value type representing all stack values *)
type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Name of string
  | Closure of string * string list * (string * value) list list * bool
    (* param, code (list of commands), env snapshot, is_inout *)
  | Error
  | Unit

type env = (string * value) list list (*each frame is a scope*)

(*string representation for output/println*)
let value_to_string = function
  | Int n -> string_of_int n
  | Float f ->
      if f = float_of_int (int_of_float f) then
        string_of_int (int_of_float f)
      else
        string_of_float f
  | Bool true -> ":true:"
  | Bool false -> ":false:"
  | Str s -> s
  | Name n -> n
  | Closure _ -> ":fun:"    (* closures rendered as :fun: per spec *)
  | Error -> ":error:"
  | Unit -> ":unit:"

(*strip surrounding quotes from a string*)
let strip_quotes str =
  let len = String.length str in
  if len >= 2 && str.[0] = '"' && str.[len - 1] = '"' then
    String.sub str 1 (len - 2)
  else str

(*check if a string is a valid integer, return option *)
let parse_int_opt str =
  try Some (int_of_string str) with _ -> None

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
      match parse_int_opt str with
      | Some n -> Some (Int n)
      | None ->
        if String.contains str '.' then
          (match parse_float_opt str with
           | Some f -> Some (Float f)
           | None ->
             if is_valid_name str then Some (Name str) else None)
        else
          if is_valid_name str then Some (Name str)
          else None

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
      then (List.map (fun (n, v) -> if n = name then (n, value) else (n, v)) frame)::rest
      else frame :: env_helper rest
  in
  env_helper env

(*resolve names to stored values*)
let rec resolve value env =
  match value with
  | Name n -> (match env_lookup n env with Some v -> v | None -> Error)
  | v -> v

let numeric_to_float = function
  | Int i -> float_of_int i
  | Float f -> f
  | _ -> failwith "numeric_to_float expects a numeric value"

let is_int = function Int _ -> true | _ -> false
let is_float = function Float _ -> true | _ -> false
let is_numeric = function Int _ | Float _ -> true | _ -> false

let add stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int y -> Int (x + y) :: tl
      | Int x, Float y -> Float (float_of_int x +. y) :: tl
      | Float x, Int y -> Float (x +. float_of_int y) :: tl
      | Float x, Float y -> Float (x +. y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let sub stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int y -> Int (x - y) :: tl
      | Int x, Float y -> Float (float_of_int x -. y) :: tl
      | Float x, Int y -> Float (x -. float_of_int y) :: tl
      | Float x, Float y -> Float (x -. y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let mult stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int y -> Int (x * y) :: tl
      | Int x, Float y -> Float (float_of_int x *. y) :: tl
      | Float x, Int y -> Float (x *. float_of_int y) :: tl
      | Float x, Float y -> Float (x *. y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let div stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int 0 -> Error :: Int 0 :: Int x :: tl
      | Int x, Int y -> Int (x / y) :: tl
      | Int x, Float y ->
          if y = 0.0 then Error :: Float 0.0 :: Int x :: tl
          else Float (float_of_int x /. y) :: tl
      | Float x, Int 0 -> Error :: Int 0 :: Float x :: tl
      | Float x, Int y -> Float (x /. float_of_int y) :: tl
      | Float x, Float y ->
          if y = 0.0 then Error :: Float 0.0 :: Float x :: tl
          else Float (x /. y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let rem stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int 0 -> Error :: Int 0 :: Int x :: tl
      | Int x, Int y -> Int (x mod y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let sign stack env =
  match stack with
  | v :: tl ->
    (match resolve v env with
      | Int x -> Int (if x = 0 then 0 else -x) :: tl
      | Float f -> Float (-. f) :: tl
      | _ -> v :: Error :: tl)
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
  | value :: tl ->
      let to_print =
        match value with
        | Str s -> s
        | _ -> value_to_string value
      in
      output_string out_file (to_print ^ "\n");
      tl
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
  | [v] -> Error ::  v :: []
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
       | Int x, Float y -> Bool (float_of_int x = y) :: tl
       | Float x, Int y -> Bool (x = float_of_int y) :: tl
       | _ -> Error ::  v2 :: v1 :: tl)
  | [v] -> Error ::  v :: []
  | [] -> [Error]

let less_than_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
      (match resolve v1 env, resolve v2 env with
       | Int x, Int y -> Bool (x < y) :: tl
       | Float x, Float y -> Bool (x < y) :: tl
       | Int x, Float y -> Bool (float_of_int x < y) :: tl
       | Float x, Int y -> Bool (x < float_of_int y) :: tl
       | _ -> Error :: v2 :: v1 :: tl)
  | [v] -> Error ::  v :: []
  | [] -> [Error]

let assign stack env =
  match stack with
  | v :: Name n :: tl ->
      let resolve_v =
        match v with
        | Name name ->
          (match env_lookup name env with
            | Some value ->
              if value = Error then None else Some value
            | None -> None)
        | Error -> None
        | Closure _ -> Some v
        | _ -> Some v
      in
      (match resolve_v with
       | Some value -> Unit :: tl, env_add n value env
       | None -> Error :: v :: Name n :: tl, env
      )
  | v :: n :: tl ->  Error :: v :: n :: tl, env
  | [v] -> Error :: v :: [], env
  | [] -> [Error], env

let if_op stack env =
  match stack with
  | x :: y :: z :: tl ->
      (match x, y, resolve z env with
      | vx, vy, Bool cond ->
         if cond then vx :: tl, env else vy :: tl, env
      | _, _, _ -> Error :: x :: y :: z :: tl, env)
  | [a; b] -> Error :: a :: b :: [], env
  | [a] -> Error :: a :: [], env
  | [] -> [Error], env

(* execute a list of commands with given initial stack, stack_env, env; stops on 'return' or end.
   returns final stack, final stack_env, final env, and a flag indicating whether a return occurred. *)
let rec exec_commands initial_stack initial_stack_env initial_env commands out_file =
  let rec loop stack stack_env env cmds =
    match cmds with
    | [] -> (stack, stack_env, env, false)
    | cmd :: rest ->
        let stripped = String.trim cmd in
        if String.length stripped = 0 then loop stack stack_env env rest
        else
          let words = String.split_on_char ' ' stripped in
          match words with
          | ["fun"; fname; param] ->
              let rec collect_body acc rem =
                match rem with
                | [] -> (List.rev acc, [])
                | h :: t ->
                    if String.trim h = "funEnd" then (List.rev acc, t)
                    else collect_body (h::acc) t
              in
              let (body, after) = collect_body [] rest in
              let closure_env = env in
              let closure_value = Closure (param, body, closure_env, false) in
              let new_stack = Unit :: stack in
              let new_env = env_add fname closure_value env in
              loop new_stack stack_env new_env after
          | ["inOutFun"; fname; param] ->
              let rec collect_body acc rem =
                match rem with
                | [] -> (List.rev acc, [])
                | h :: t ->
                    if String.trim h = "funEnd" then (List.rev acc, t)
                    else collect_body (h::acc) t
              in
              let (body, after) = collect_body [] rest in
              let closure_env = env in
              let closure_value = Closure (param, body, closure_env, true) in
              let new_stack = Unit :: stack in
              let new_env = env_add fname closure_value env in
              loop new_stack stack_env new_env after
          | ["funEnd"] ->
              loop stack stack_env env rest
          | _ ->
              let updated_stack, updated_stack_env, updated_env, stop_flag =
                interpret_command stack stack_env env stripped out_file
              in
              if stop_flag then (updated_stack, updated_stack_env, updated_env, true)
              else loop updated_stack updated_stack_env updated_env rest
  in
  loop initial_stack initial_stack_env initial_env commands

(* interpret a single command (non-block commands). returns new stack and env and stop-flag. *)
and interpret_command (stack : value list) (stack_env : value list list) (env : env) (cmd : string) (out_file : out_channel)
    : value list * value list list * env * bool =
  if String.length cmd = 0 then (stack, stack_env, env, false)
  else
    let words = String.split_on_char ' ' cmd in
    match words with
    | ["push"; arg] -> (push stack arg, stack_env, env, false)
    | "push" :: rest ->
        let arg = String.concat " " rest in
        (push stack arg, stack_env, env, false)
    | ["pop"] -> (pop stack, stack_env, env, false)
    | ["add"] -> (add stack env, stack_env, env, false)
    | ["sub"] -> (sub stack env, stack_env, env, false)
    | ["mult"] -> (mult stack env, stack_env, env, false)
    | ["div"] -> (div stack env, stack_env, env, false)
    | ["rem"] -> (rem stack env, stack_env, env, false)
    | ["sign"] -> (sign stack env, stack_env, env, false)
    | ["swap"] -> (swap stack, stack_env, env, false)
    | ["toString"] -> (to_string_stack stack, stack_env, env, false)
    | ["println"] -> (println stack out_file, stack_env, env, false)
    | ["cat"] -> (cat stack env, stack_env, env, false)
    | ["and"] -> (and_op stack env, stack_env, env, false)
    | ["or"] -> (or_op stack env, stack_env, env, false)
    | ["not"] -> (not_op stack env, stack_env, env, false)
    | ["equal"] -> (equal_op stack env, stack_env, env, false)
    | ["lessThan"] -> (less_than_op stack env, stack_env, env, false)
    | ["assign"] ->
        let s,e = assign stack env in (s, stack_env, e, false)
    | ["if"] ->
        let s,e = if_op stack env in (s, stack_env, e, false)

    (* Environment handling: Added let/end for block scope *)
    | ["let"] -> ([], stack :: stack_env, [] :: env, false)
    | ["end"] ->
        (match stack_env with
         | outer_stack :: rest_stack_env ->
            let top =
              match stack with
              | v :: _ -> v      (*take top value of inner stack*)
              | [] -> Error      (*if inner stack empty*)
            in
            (top :: outer_stack,
             rest_stack_env,
             (match env with _ :: rest_env -> rest_env | [] -> []),
             false)
         | [] -> (Error :: stack, [], env, false))

    | ["call"] ->
        (match stack with
         | arg :: Name fname :: tl ->
            (match env_lookup fname env with
             | Some (Closure (param, code, closure_env, is_inout)) ->
                 let arg_value =
                   match arg with
                   | Name aname -> (match env_lookup aname env with Some v -> v | None -> Error)
                   | Error -> Error
                   | _ -> arg
                 in
                 if arg_value = Error then
                   Error :: arg :: Name fname :: tl, stack_env, env, false
                 else
                   let saved_env = env in
                   let saved_stack = tl in
                   let fn_env =
                     match closure_env with
                     | [] -> [[(param, arg_value)]]
                     | _ -> env_add param arg_value closure_env
                   in
                   let new_stack_env = (saved_stack) :: stack_env in
                   let fn_stack = [] in
                   let (fn_final_stack, fn_stack_env_after, fn_env_after, returned_flag) =
                     exec_commands fn_stack new_stack_env fn_env code out_file
                   in
                   let caller_stack =
                     match fn_stack_env_after with
                     | [] -> saved_stack
                     | caller_stack :: _ -> caller_stack
                   in
                   let push_val =
                     match fn_final_stack with
                     | v :: _ -> v
                     | [] -> Error
                   in
                   let final_caller_env =
                     if is_inout then
                       (match arg with
                        | Name actual_name ->
                            (match env_lookup param fn_env_after with
                             | Some vparam -> env_update actual_name vparam saved_env
                             | None -> saved_env)
                        | _ -> saved_env)
                     else saved_env
                   in
                   (push_val :: caller_stack, (match fn_stack_env_after with | [] -> [] | _::rest -> rest), final_caller_env, false)
             | Some _ ->
                 Error :: arg :: Name fname :: tl, stack_env, env, false
             | None ->
                 Error :: arg :: Name fname :: tl, stack_env, env, false)
         | arg :: Closure (param, code, closure_env, is_inout) :: tl ->
             let arg_value =
               match arg with
               | Name aname -> (match env_lookup aname env with Some v -> v | None -> Error)
               | Error -> Error
               | _ -> arg
             in
             if arg_value = Error then
               Error :: arg :: (Closure (param, code, closure_env, is_inout)) :: tl, stack_env, env, false
             else
               let saved_env = env in
               let saved_stack = tl in
               let fn_env = env_add param arg_value closure_env in
               let new_stack_env = (saved_stack) :: stack_env in
               let fn_stack = [] in
               let (fn_final_stack, fn_stack_env_after, fn_env_after, _) =
                 exec_commands fn_stack new_stack_env fn_env code out_file
               in
               let caller_stack =
                 match fn_stack_env_after with
                 | [] -> saved_stack
                 | caller_stack :: _ -> caller_stack
               in
               let push_val =
                 match fn_final_stack with
                 | v :: _ -> v
                 | [] -> Error
               in
               let final_caller_env = saved_env in
               (push_val :: caller_stack, (match fn_stack_env_after with | [] -> [] | _::rest -> rest), final_caller_env, false)
         | _ -> Error :: stack, stack_env, env, false)
    | ["return"] ->
        (stack, stack_env, env, true)
    | _ -> (stack, stack_env, env, false)

(* interpret a single command at top-level which may include function declarations *)
let interpret_top_command (stack : value list) (stack_env : value list list) (env : env) (cmd : string) (rest_commands : string list) (out_file : out_channel)
    : value list * value list list * env * string list =
  let stripped = String.trim cmd in
  if String.length stripped = 0 then (stack, stack_env, env, rest_commands)
  else
    let words = String.split_on_char ' ' stripped in
    match words with
    | ["fun"; fname; param] ->
        let rec collect acc rem =
          match rem with
          | [] -> (List.rev acc, [])
          | h :: t ->
              if String.trim h = "funEnd" then (List.rev acc, t)
              else collect (h :: acc) t
        in
        let (body, remaining_after) = collect [] rest_commands in
        let closure_env = env in
        let closure_value = Closure (param, body, closure_env, false) in
        let new_stack = Unit :: stack in
        let new_env = env_add fname closure_value env in
        (new_stack, stack_env, new_env, remaining_after)
    | ["inOutFun"; fname; param] ->
        let rec collect acc rem =
          match rem with
          | [] -> (List.rev acc, [])
          | h :: t ->
              if String.trim h = "funEnd" then (List.rev acc, t)
              else collect (h :: acc) t
        in
        let (body, remaining_after) = collect [] rest_commands in
        let closure_env = env in
        let closure_value = Closure (param, body, closure_env, true) in
        let new_stack = Unit :: stack in
        let new_env = env_add fname closure_value env in
        (new_stack, stack_env, new_env, remaining_after)
    | _ ->
        let s, se, e, _ = interpret_command stack stack_env env stripped out_file in
        (s, se, e, rest_commands)

(*main function *)
let interpreter ((input : string), (output : string)) : unit =
  let in_file = open_in input in
  let out_file = open_out output in

  let rec read_lines acc =
    try read_lines (input_line in_file :: acc)
    with End_of_file -> List.rev acc
  in
  let commands = read_lines [] in

  let rec process stack stack_env env cmds =
    match cmds with
    | [] -> stack
    | cmd :: rest ->
        let stripped = String.trim cmd in
        if stripped = "quit" then stack
        else
          let updated_stack, updated_stack_env, updated_env, remaining = interpret_top_command stack stack_env env stripped rest out_file in
          process updated_stack updated_stack_env updated_env remaining
  in
  ignore(process [] [] [[]] commands);
  close_in in_file;
  close_out out_file
;;

(* Run using your example input file paths, or change as needed *)
interpreter ("input10.txt", "output10.txt");;
