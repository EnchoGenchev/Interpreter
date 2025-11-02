(*Encho Genchev 113570835*)

(*value type representing all stack values *)
type value =
  | Int of int
  | Bool of bool
  | Str of string
  | Name of string
  | Error
  | Unit


type env = (string * value) list list (*each frame is a scope*)

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

let rec env_lookup name env =
  match env with
  | [] -> None
  | frame :: rest -> (*starts at start of list to check inner frames first*)
    match List.assoc_opt name frame with (*function that looks up name of variable in that frame*)
    | Some v -> Some v
    | None -> env_lookup name rest (*if not found then checks outer stack*)

let env_add name value env =
  match env with
  | frame :: rest -> ((name, value)::frame) :: rest (*adds assigned variables to corresponding environment*)
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
  | Name n -> (match env_lookup n env with Some v -> v | None -> Error) (*this format to get rid of nonexhaustive pattern match*)
  | v -> v (*if it's not a name then just returns the value*)


let add stack env = (* env added *)
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with (*get values from names if applicable*)
      | Int x, Int y -> Int (x + y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  (*doesn't work with less than two values*)
  | [v] -> v :: Error :: [] 
  | [] -> [Error]

let sub stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with (*get values from names if applicable*)
      | Int x, Int y -> Int (x - y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  (*doesn't work with less than two values*)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let mult stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with (*get values from names if applicable*)
      | Int x, Int y -> Int (x * y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  (*doesn't work with less than two values*)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let div stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with (*get values from names if applicable*)
      | Int x, Int 0 -> Error :: Int 0 :: Int x :: tl
      | Int x, Int y -> Int (x / y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  (*doesn't work with less than two values*)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let rem stack env =
  match stack with 
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with (*get values from names if applicable*)
      | Int x, Int 0 -> Error :: Int 0 :: Int x :: tl
      | Int x, Int y -> Int (x mod y) :: tl
      | _ -> v2 :: v1 :: Error :: tl)
  (*doesn't work with less than two values*)
  | [v] -> v :: Error :: []
  | [] -> [Error]

let sign stack env =
  match stack with
  | v :: tl ->
    (match resolve v env with (*get values from names if applicable*)
      | Int x -> Int (if x = 0 then 0 else -x) :: tl
      | _ -> v :: Error :: tl) (*doesn't work unless value is an int*)
  (*doesn't work without any values*)
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

let cat stack env =
  match stack with
  | Str y :: Str x :: tl -> Str (x ^ y) :: tl (*concatenating both strings*)
  (*doesn't work with less than two values that aren't strings*)
  | v1 :: v2 :: tl -> v2 :: v1 :: Error :: tl
  | [v] -> v :: Error :: []
  | [] -> [Error]

let and_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Bool x, Bool y -> Bool (x && y) :: tl (*only works if top two values are booleans*)
      | _ -> Error :: v2 :: v1 :: tl) (*otherwise just pushes values back with error*)
  | [v] -> Error :: v :: []
  | [] -> [Error]

let or_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
    (match resolve v1 env, resolve v2 env with
      | Bool x, Bool y -> Bool (x || y) :: tl (*only works if top two values are booleans*)
      | _ -> Error :: v2 :: v1 :: tl) (*otherwise just pushes values back with error*)
  | [v] -> Error ::  v :: []
  | [] -> [Error]

let not_op stack env =
  match stack with
  | v :: tl ->
    (match resolve v env with
      | Bool x -> Bool (not x) :: tl (*only works if top value is boolean*)
      | _ -> Error :: v :: tl) (*otherwise just pushes value back with error*)
  | [] -> [Error]

let equal_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
      (match resolve v1 env, resolve v2 env with
       | Int x, Int y -> Bool (x = y) :: tl (*compares two ints and returns result casts as Bool*)
       | _ -> Error ::  v2 :: v1 :: tl)
  | [v] -> Error ::  v :: []
  | [] -> [Error]

let less_than_op stack env =
  match stack with
  | v2 :: v1 :: tl ->
      (match resolve v1 env, resolve v2 env with
       | Int x, Int y -> Bool (x < y) :: tl (*compares two ints and returns result casts as Bool*)
       | _ -> Error :: v2 :: v1 :: tl)
  | [v] -> Error ::  v :: []
  | [] -> [Error]

let assign stack env =
  match stack with
  | v :: Name n :: tl ->
      let resolve_v = match v with
        | Name name ->
          (match env_lookup name env with
            | Some value ->
              if value = Error (*don't want to assign error to a name*)
                then None (*using none for easy pattern matching later*)
              else Some value (*assign the value to the new name if valid *)
            | None -> None)
        | Error -> None
        | _ -> Some v (*at this point v is a Bool, int, or string*)
      in 
      (match resolve_v with
       | Some value -> Unit :: tl, env_add n value env (*if v was assigned then push unit to stack*)
       | None -> Error ::  Name n :: v :: tl, env (*if not then push error and values back to stack*)
      )
  | v :: n :: tl ->  Error :: v :: n :: tl, env
  | [v] -> Error :: v :: [], env
  | [] -> [Error], env

let if_op stack env =
  match stack with
  | x :: y :: z :: tl -> 
      (match resolve x env, resolve y env, resolve z env with (*looks for values in whatever stack they may be in*)
      | vx, vy, Bool cond -> (*only works if third value is a Bool*)
         if cond then vx :: tl, env else vy :: tl, env
  (*otherwise pushes value back with error*)
      | _, _, _ -> Error :: x :: y :: z :: tl, env)
  | [a; b] -> Error :: a :: b :: [], env
  | [a] -> Error :: a :: [], env
  | [] -> [Error], env

(*interpret a single command, returns new stack *)
let interpret_command (stack : value list) (stack_env : value list list) (env : env) (cmd : string) (out_file : out_channel) 
    : value list * value list list * env =
  if String.length cmd = 0 then (stack, stack_env, env) (*return the stack once there are no more commands*)
  else
    let words = String.split_on_char ' ' cmd in (*split command string into words*)
    match words with
    (*IMPORTANT: return the result of the command as well as the updated stack and environment so that following commands run correctly*)
    (*basic stack commands*)
    | ["push"; arg] -> (push stack arg, stack_env, env)
    | "push" :: rest -> 
        let arg = String.concat " " rest in (*for strings with spaces*)
        (push stack arg, stack_env, env)
    | ["pop"] -> (pop stack, stack_env, env)
    | ["add"] -> (add stack env, stack_env, env)
    | ["sub"] -> (sub stack env, stack_env, env)
    | ["mult"] -> (mult stack env, stack_env, env)
    | ["div"] -> (div stack env, stack_env, env)
    | ["rem"] -> (rem stack env, stack_env, env)
    | ["sign"] -> (sign stack env, stack_env, env)
    | ["swap"] -> (swap stack, stack_env, env)
    | ["toString"] -> (to_string_stack stack, stack_env, env)
    | ["println"] -> (println stack out_file, stack_env, env)
    (*boolean and comparison stuff*)
    | ["cat"] -> (cat stack env, stack_env, env)
    | ["and"] -> (and_op stack env, stack_env, env)
    | ["or"] -> (or_op stack env, stack_env, env)
    | ["not"] -> (not_op stack env, stack_env, env)
    | ["equal"] -> (equal_op stack env, stack_env, env)
    | ["lessThan"] -> (less_than_op stack env, stack_env, env)
    | ["assign"] -> let s,e = assign stack env in (s, stack_env, e)
    | ["if"] -> let s,e = if_op stack env in (s, stack_env, e)
    (* Environment handling *)
    | ["let"] -> ([], stack :: stack_env, [] :: env)  (*push current stack onto stack_env, start new empty stack*)
    | ["end"] ->
        (match stack_env with
        | outer_stack :: rest_stack_env ->
            let top =
              match stack with
              | v :: _ -> v      (*take top value of inner stack*)
              | [] -> Error       (*if inner stack empty*)
            in
            (top :: outer_stack, rest_stack_env, match env with _ :: rest_env -> rest_env | [] -> [] )
        | [] -> (Error :: stack, [], env))  (*no outer stack*)
    | _ -> (stack, stack_env, env)



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

  let rec process stack stack_env env commands =
    match commands with
    | [] -> stack
    | cmd :: rest ->
      let stripped = String.trim cmd in  (*trim whitespace and newlines so it reads correctly*)
      if stripped = "quit" then stack (*return stack*)
      else
        let updated_stack, updated_stack_env, updated_env = interpret_command stack stack_env env stripped out_file in
        process updated_stack updated_stack_env updated_env rest (*runs rest of commands*)
  in
  (*this is the empty stack and empty stack-of-stacks that we start with*)
  ignore(process [] [] [[]] commands); (*initial empty stack, stack_env, and global env that can be passed to commands in process function*)
  close_in in_file;
  close_out out_file
;;


interpreter ("input.txt", "output.txt");
