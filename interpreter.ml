(* Value type representing all stack values *)
type value =
  | Int of int
  | Bool of bool
  | Str of string
  | Name of string
  | Error
  | Unit

(* String representation for output/println, as specified *)
let value_to_string = function
  | Int n -> string_of_int n
  | Bool true -> ":true:"
  | Bool false -> ":false:"
  | Str s -> s
  | Name n -> n
  | Error -> ":error:"
  | Unit -> ":unit:"

(* Helper: strip surrounding quotes from a string *)
let strip_quotes s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"'
  then String.sub s 1 (len - 2)
  else s

(* Check if a string is a valid integer, return option *)
let parse_int_opt s =
  try Some (int_of_string s) with _ -> None

let is_valid_name s =
  let len = String.length s in
  if len = 0 then false
  else
    let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') in
    let is_digit c = '0' <= c && c <= '9' in
    let is_underscore c = c = '_' in
    (is_letter s.[0] || is_underscore s.[0]) &&
    String.for_all (fun c -> is_letter c || is_digit c || is_underscore c) s

let parse_const s =
  match s with
    | ":true:" -> Some (Bool true)
    | ":false:" -> Some (Bool false)
    | ":error:" -> Some Error
    | ":unit:" -> Some Unit
    | _ when String.length s >= 2 && s.[0] = '"' && s.[String.length s-1] = '"' ->
        Some (Str (strip_quotes s))
    | _ ->
      (match parse_int_opt s with
       | Some n -> Some (Int n)
       | None ->
         if is_valid_name s then Some (Name s)
         else None)

let push_const stack s =
  match parse_const s with
  | Some v -> v :: stack
  | None -> Error :: stack

let pop stack =
  match stack with
  | [] -> [Error]
  | _::tl -> tl

let binop stack f =
  match stack with
  | Int y :: Int x :: tl -> (Int (f x y)) :: tl
  | v1 :: v2 :: tl -> v2 :: v1 :: Error :: tl
  | [v] -> v :: Error :: []
  | [] -> [Error]

let add stack = binop stack ( + )
let sub stack = binop stack ( - )
let mult stack = binop stack ( * )
let div stack =
  match stack with
  | Int 0 :: Int x :: tl -> Int x :: Int 0 :: Error :: tl
  | Int y :: Int x :: tl -> Int (x / y) :: tl
  | v1 :: v2 :: tl -> v2 :: v1 :: Error :: tl
  | [v] -> v :: Error :: []
  | [] -> [Error]

let rem stack =
  match stack with
  | Int 0 :: Int x :: tl -> Int x :: Int 0 :: Error :: tl
  | Int y :: Int x :: tl -> Int (x mod y) :: tl
  | v1 :: v2 :: tl -> v2 :: v1 :: Error :: tl
  | [v] -> v :: Error :: []
  | [] -> [Error]

let sign stack =
  match stack with
  | Int x :: tl -> Int (if x = 0 then 0 else -x) :: tl
  | v :: tl -> v :: Error :: tl
  | [] -> [Error]

let swap stack =
  match stack with
  | v1 :: v2 :: tl -> v2 :: v1 :: tl
  | [v] -> v :: Error :: []
  | [] -> [Error]

let to_string_stack stack =
  match stack with
  | v :: tl -> Str (value_to_string v) :: tl
  | [] -> [Error]

let println stack out_chan =
  match stack with
  | v :: tl ->
    output_string out_chan (value_to_string v ^ "\n");
    tl
  | [] ->
    [Error]

(* Interpret a single command, returns new stack *)
let interpret_command (stack : value list) (cmd : string) (out_chan : out_channel) : value list =
  let cmd = String.trim cmd in
  if String.length cmd = 0 then stack
  else
    let words = String.split_on_char ' ' cmd in
    match words with
    | ["push"; arg] ->
      push_const stack arg
    | ["pop"] -> pop stack
    | ["add"] -> add stack
    | ["sub"] -> sub stack
    | ["mult"] -> mult stack
    | ["div"] -> div stack
    | ["rem"] -> rem stack
    | ["sign"] -> sign stack
    | ["swap"] -> swap stack
    | ["toString"] -> to_string_stack stack
    | ["println"] -> println stack out_chan
    (* allow choose push with quoted string arg with spaces: push "hello world" *)
    | "push"::rest when List.length rest > 0 ->
      let arg = String.concat " " rest in
      push_const stack arg
    | _ -> stack  (* ignore unknown/empty/quit here *)
    

(* Main interpreter function *)
let interpreter ((input : string), (output : string)) : unit =
  let in_chan = open_in input in
  let out_chan = open_out output in
  let rec read_lines acc =
    try read_lines (input_line in_chan :: acc) with End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  let rec process stack commands =
    match commands with
    | [] -> stack
    | cmd :: rest ->
      let stripped = String.trim cmd in
      if stripped = "quit" then stack
      else
        let s = interpret_command stack stripped out_chan in
        process s rest
  in
  ignore (process [] lines);
  close_in in_chan;
  close_out out_chan
;;

interpreter ("input10.txt", "output10.txt");;