(*value type representing all stack values *)
type value =
  | Int of int
  | Bool of bool
  | Str of string
  | Name of string
  | Error
  | Unit

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

let add stack =
  match stack with
  (*if top two values are integers*)
  | Int y :: Int x :: tl -> Int (x + y) :: tl 
  (*top two values are not integers*)
  | v1 :: v2 :: tl -> Error :: v1 :: v2 :: tl (*preserves stack but pushes error*)
  (*only one element in stack*)
  | [v] -> Error :: v :: [] (*pushes Error to the stack after the element*)
  (*empty stack*)
  | [] -> [Error] (*pushes error to the stack*)

let sub stack = (*same as addition but with - symbol instead*)
  match stack with
  (*top two values are integers*)
  | Int y :: Int x :: tl -> Int (x - y) :: tl 
  (*top two values are not integers*)
  | v1 :: v2 :: tl -> Error :: v1 :: v2  :: tl
  (*only one element in stack*)
  | [v] -> v :: Error :: []
  (*empty stack*)
  | [] -> [Error]

let mult stack = (*same as add and sub but with * symbol*)
  match stack with
  (*top two values are integers*)
  | Int y :: Int x :: tl -> Int (x * y) :: tl
  (*top two values are not integers*)
  | v1 :: v2 :: tl -> Error  :: v1 :: v2 :: tl
  (*only one element in stack*)
  | [v] -> v :: Error :: []
  (*empty stack*)
  | [] -> [Error]

let div stack = (*like add, sub, and mult but with extra condition*)
  match stack with
  (*division by 0*)
  | Int 0 :: Int x :: tl -> Error :: Int x :: Int 0 :: tl 
  (*top two values are integers*)
  | Int y :: Int x :: tl -> Int (x / y) :: tl (*pushes x/y to the stack (rounds to nearest int)*)
  (*top two values are not integers*)
  | v1 :: v2 :: tl ->  Error :: v1 :: v2 :: tl
  (*only one element in stack*)
  | [v] -> v :: Error :: []
  (*empty stack*)
  | [] -> [Error]


let rem stack = (*like add, sub, and mult but using mod*)
  match stack with 
  (*0 mod anything is not valid because it's like division by 0*)
  | Int 0 :: Int x :: tl -> Error :: Int 0 :: Int x ::  tl
   (*top two values are integers*)
  | Int y :: Int x :: tl -> Int (x mod y) :: tl
  (*top two values are not integers*)
  | v1 :: v2 :: tl -> Error ::  v1 :: v2 :: tl
  (*only one element in stack*)
  | [v] -> v :: Error :: []
  (*empty stack*)
  | [] -> [Error]

let sign stack =
  match stack with
  | Int x :: tl -> Int (if x = 0 then 0 else -x) :: tl (*0 doesn't have a sign otherwise push -x*)
  | value :: tl -> Error :: value :: tl (*if not an integer*)
  | [] -> [Error] (*if stack empty*)

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

(*interpret a single command, returns new stack *)
let interpret_command (stack : value list) (cmd : string) (out_file : out_channel) : value list =
  if String.length cmd = 0 then stack (*return the stack once there are no more commands*)
  else
    let words = String.split_on_char ' ' cmd in (*splits words and puts in string list*)
    match words with
    | ["push"; arg] -> push stack arg 
    | ["pop"] -> pop stack
    | ["add"] -> add stack
    | ["sub"] -> sub stack
    | ["mult"] -> mult stack
    | ["div"] -> div stack
    | ["rem"] -> rem stack
    | ["sign"] -> sign stack
    | ["swap"] -> swap stack
    | ["toString"] -> to_string_stack stack
    | ["println"] -> println stack out_file
    | "push" :: rest -> let arg = String.concat " " rest in push stack arg (*for strings with spaces*)
    | _ -> stack
    

(*main function *)
let interpreter ((input : string), (output : string)) : unit =
  let in_file = open_in input in
  let out_file = open_out output in

  let rec read_lines acc =
    (*prepend lines recursively until reach the end of the file*)
    try read_lines (input_line in_file :: acc) 
    with End_of_file -> List.rev acc (*reverse cuz prepended*)
  in
  let lines = read_lines [] in

  let rec process stack commands =
    match commands with
    | [] -> stack
    | cmd :: rest ->
      let stripped = String.trim cmd in  (*trim whitespace and newlines so it reads correctly*)
      if stripped = "quit" then stack
      else
        let s = interpret_command stack stripped out_file in
        process s rest
  in

  ignore(process [] lines); (*don't need the stack anymore*)
  close_in in_file;
  close_out out_file
;;

interpreter ("input5.txt", "output5.txt");;
