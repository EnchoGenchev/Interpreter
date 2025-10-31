(*value type representing all stack values *)
type value =
  | Int of int
  | Bool of bool
  | Str of string
  | Name of string
  | Error
  | Unit

(* ADDED FOR PART 2: Environment type for variable scopes *)
type env = (string * value) list list (* each frame is a scope *)

(*string representation for output/println*)
let value_to_string = function
  | Int n -> string_of_int n
  | Bool true -> ":true:"
  | Bool false -> ":false:"
  | Str s -> s
  | Name n -> n
  | Error -> ":error:"
  | Unit -> ":unit:"

(*strip surrounding quotes from a string*)
let strip_quotes str =
  let len = String.length str in
  if len >= 2 && str.[0] = '"' && str.[len - 1] = '"'
  then String.sub str 1 (len - 2) (*gets substring between first and last index, aka excludes quotes*)
  else str (*if doesn't have quotes just return the string*)

(*check if a string is a valid integer, return option *)
let parse_int_opt str =
  try Some (int_of_string str) with _ -> None (*if function doesn't work returns one*)

let is_valid_name str =
  let len = String.length str in
  if len = 0 then false
  else 
    (*functions for checking name criteria*)
    let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') in
    let is_digit c = '0' <= c && c <= '9' in
    let is_underscore c = c = '_' in
    (*making sure string starts with a letter or underscore then making sure that's all there is*)
    (is_letter str.[0] || is_underscore str.[0]) &&
    (*string.for_all takes the functions along with the string as arguments
    all the functions return booleans so final return will also be boolean*)
    String.for_all (fun c -> is_letter c || is_digit c || is_underscore c) str 

let parse str =
  match str with
    (*returns value types to be pushed to stack*)
    | ":true:" -> Some (Bool true)
    | ":false:" -> Some (Bool false)
    | ":error:" -> Some Error
    | ":unit:" -> Some Unit
    | _ -> 
      if String.length str >= 2 && str.[0] = '"' && str.[String.length str - 1] = '"' then
      Some (Str (strip_quotes str)) (*returns string without quotes*)
      else
      match parse_int_opt str with (*checking if it's an int*)
      | Some n -> Some (Int n) 
      | None -> (*if not then test if it's a valid name*)
          if is_valid_name str 
            then Some (Name str)
          else None (*if not then it is not a valid value and returns an error in push*)

let push stack str =
  match parse str with (*parsing the string*)
  | Some value -> value :: stack (*adding a value to the top of the stack*)
  | None -> Error :: stack

let pop stack =
  match stack with
  | [] -> [Error] (*if stack empty then there's nothing to pop*)
  | _::tl -> tl (*if not empty then just return tail of stack*)

(* ADDED FOR PART 2: Utility functions for env *)
let rec env_lookup name env =
  match env with
  | [] -> None
  | frame :: rest ->
    match List.assoc_opt name frame with
    | Some v -> Some v
    | None -> env_lookup name rest

let env_add name value env =
  match env with
  | frame :: rest -> ((name, value)::frame) :: rest
  | [] -> [[(name, value)]]

let env_update name value env =
  let rec aux = function
    | [] -> []
    | frame :: rest ->
      if List.exists (fun (n,_) -> n = name) frame
      then (List.map (fun (n, v) -> if n = name then (n, value) else (n, v)) frame)::rest
      else frame :: aux rest
  in
  aux env

(* ADDED FOR PART 2: resolve names to bound values *)
let rec resolve value env =
  match value with
  | Name n -> (match env_lookup n env with Some v -> v | None -> Error)
  | v -> v

(* ORIGINAL add function, updated to resolve Names *)
let add stack env = (* env added *)
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int y -> Int (x + y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let sub stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int y -> Int (x - y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let mult stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int y -> Int (x * y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let div stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Int x, Int 0 -> Error :: Int 0 :: Int x :: tl
      | Int x, Int y -> Int (x / y) :: tl
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
      | _ -> v :: Error :: tl)
  | [] -> [Error]

let swap stack =
  match stack with
  | v1 :: v2 :: tl -> v2 :: v1 :: tl
  (*can't swap if there's less than 2 values in the stack*)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let to_string_stack stack =
  match stack with
  | value :: tl -> Str (value_to_string value) :: tl
  | [] -> [Error] (*can't convert nothing to a string*)

let println stack out_file =
  match stack with
  | value :: tl -> output_string out_file (value_to_string value ^ "\n");
                tl (*return the rest of the stack*)
  | [] -> (*stack empty so nothing to print*)
    [Error]

(* ADDED FOR PART 2: cat, and, or, not, equal, lessThan *)
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
       | _ -> Error ::  v2 :: v1 :: tl)
  | [v] -> Error ::  v :: []
  | [] -> [Error]

let less_than_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
      (match resolve v1 env, resolve v2 env with
       | Int x, Int y -> Bool (x < y) :: tl
       | _ -> Error :: v2 :: v1 :: tl)
  | [v] -> Error ::  v :: []
  | [] -> [Error]

let assign stack env =
  match stack with
  | v :: Name n :: tl ->
      let res_v = match v with
        | Name name ->
          (match env_lookup name env with
            | Some value ->
              if value = Error then None else Some value
            | None -> None)
        | Error -> None
        | _ -> Some v
      in
      (match res_v with
       | Some value -> Unit :: tl, env_add n value env
       | None -> Error ::  Name n :: v :: tl, env
      )
  | v :: n :: tl -> n :: v :: Error :: tl, env
  | [v] -> Error :: v :: [], env
  | [] -> [Error], env

let if_op stack env =
  match stack with
  | x :: y :: z :: tl -> 
      (match resolve x env, resolve y env, resolve z env with
      | vx, vy, Bool cond ->
         if cond then vx :: tl, env else vy :: tl, env
      | _, _, _ -> Error :: x :: y :: z :: tl, env)
  | [a; b] -> Error :: a :: b :: [], env
  | [a] -> Error :: a :: [], env
  | [] -> [Error], env

let rec concat_stack s1 s2 =
  match s1 with
  | [] -> s2
  | hd :: tl -> hd :: (concat_stack tl s2)

(*interpret a single command, returns new stack *)
(* ADDED FOR PART 2: environment is threaded through *)
let interpret_command (stack : value list) (env : env) (cmd : string) (out_file : out_channel) : value list * env =
  if String.length cmd = 0 then (stack, env) (*return the stack once there are no more commands*)
  else
    let words = String.split_on_char ' ' cmd in (*split command string into words*)
    match words with
    (* Basic stack manipulations *)
    | ["push"; arg] -> (push stack arg, env)
    | "push" :: rest -> 
        let arg = String.concat " " rest in (*handle strings with spaces*)
        (push stack arg, env)
    | ["pop"] -> (pop stack, env)
    | ["add"] -> (add stack env, env)
    | ["sub"] -> (sub stack env, env)
    | ["mult"] -> (mult stack env, env)
    | ["div"] -> (div stack env, env)
    | ["rem"] -> (rem stack env, env)
    | ["sign"] -> (sign stack env, env)
    | ["swap"] -> (swap stack, env)
    | ["toString"] -> (to_string_stack stack, env)
    | ["println"] -> (println stack out_file, env)
    (* Boolean and comparison operations *)
    | ["cat"] -> (cat stack env, env)
    | ["and"] -> (and_op stack env, env)
    | ["or"] -> (or_op stack env, env)
    | ["not"] -> (not_op stack env, env)
    | ["equal"] -> (equal_op stack env, env)
    | ["lessThan"] -> (less_than_op stack env, env)
    (* Assignment and control flow *)
    | ["assign"] -> assign stack env
    | ["if"] -> if_op stack env
    (* Environment handling *)
    | ["let"] -> (stack, [] :: env)
    | ["end"] ->
        (match env with
        | [] -> Error :: stack, env
        | frame :: rest_env -> 
            (match stack with
            | v :: tl -> [v], rest_env
            | [] -> [], rest_env))
    | _ -> (stack, env)


(*main function *)
let interpreter ((input : string), (output : string)) : unit =
  let in_file = open_in input in
  let out_file = open_out output in

  let rec read_lines acc =
    (*prepend lines recursively until reach the end of the file*)
    try read_lines (input_line in_file :: acc) 
    with End_of_file -> List.rev acc (*reverse cuz prepended*)
  in
  let commands = read_lines [] in

  (* ADDED FOR PART 2: env threaded in process *)
  let rec process stack env commands =
    match commands with
    | [] -> stack
    | cmd :: rest ->
      let stripped = String.trim cmd in  (*trim whitespace and newlines so it reads correctly*)
      if stripped = "quit" then stack (*return stack*)
      else
        let s, e = interpret_command stack env stripped out_file in
        process s e rest
  in
  (*this is the empty stack that we start with and then run the commands on it*)
  ignore(process [] [[]] commands); (* PART 2: initial global environment *)
  close_in in_file;
  close_out out_file
;;

interpreter ("input0.txt", "output0.txt");