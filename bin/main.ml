type 'a stats =
  | HP : 'a -> 'a stats
  | XP : float stats
  | IntHp : int -> int stats


let s = HP 1
let t = XP
let u = IntHp 1

let f : type a.a stats -> _ = function
  | HP _ -> 1
  | IntHp s -> s
  | XP -> 1

let () =
  print_int (f s); print_newline ();
  print_int (f t); print_newline ();
  print_int (f u); print_newline ();
  print_newline ();

open Gadts.VisitorGADT

let rec eval : type a. a term -> a = function
  | Int n -> n
  | Add -> (fun x y -> x + y)
  | App (f,x) -> (eval f) (eval x)

let two = eval (App (Add, Int 1))

let rec sum : type a. a term -> _ = fun x ->
  let y =
    match x with
    | Int n -> n
    | Add -> 0
    | App(f,x) -> sum f + sum x
  in y

let get_int : int term -> int = function
  | Int n -> n
  | App(_,_) -> 0

let () =
  print_int (two 3); print_newline ();
  print_int (sum (App (App (Add, Int 1), Int 1))); print_newline ();
  print_int (get_int (App (App (Add, Int 1), Int 1))); print_newline ();

type _ t =
  | Int : int t
  | Bool : bool t

let deep : (char t * int) option -> char = function
  | None -> 'c'
  | _ -> .

let nothing : type a.a t -> unit = function
  | Bool -> ()
  | Int -> ()

let () =
  print_char (deep None); print_newline ();
  nothing Bool;
  nothing Int;
  print_newline ();

type _ typ =
  | Int : int typ
  | String : string typ
  | Pair : 'a typ * 'b typ -> ('a * 'b) typ

let rec to_string : type a. a typ -> a -> string = fun t x ->
  match t with
  | Int -> Int.to_string x
  | String -> Printf.sprintf "%S" x
  | Pair(t1, t2) ->
    let (x1,x2) = x in
    Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2)

let () =
  print_endline (to_string Int 1);
  print_endline (to_string String "Hello World");
  print_endline (to_string (Pair (Int, String)) (2, "Bye"));
  print_newline ();

type (_,_) eq = Eq : ('a,'a) eq

type (_,_) eq' = 
  | Eq' : ('a,'a) eq'
  | IntString : (int, string) eq'
  | IntChar : (int, char) eq'


let cast : type a b. (a,b) eq -> a -> b = fun Eq x -> x

let cast' : type a b. (a,b) eq' -> a -> b = fun e x ->
  match e with
  | Eq' -> x
  | IntString -> string_of_int x
  | IntChar -> char_of_int x

let rec eq_type : type a b. a typ -> b typ -> (a,b) eq option =
  fun a b ->
  match a,b with
  | Int, Int -> Some Eq
  | String, String -> Some Eq
  | Pair (a1,a2), Pair (b1, b2) -> 
    begin
      match eq_type a1 b1, eq_type a2 b2 with
      | Some Eq, Some Eq -> Some Eq
      | _ -> None
    end
  | _ -> None

type dyn = Dyn : 'a typ * 'a -> dyn

let get_dyn : type a. a typ -> dyn -> a option =
  fun a (Dyn(b,x)) ->
  match eq_type a b with
  | None -> None
  | Some Eq -> Some x


let () =
  print_endline (match eq_type Int Int with
  | Some Eq -> "match"
  | None -> "not a match");
  print_endline (match get_dyn String (Dyn (String, "hello")) with
  | Some v -> v
  | None -> "not a match");
  print_endline (cast Eq "hello");
  print_endline (cast' Eq' "hello");
  print_endline (cast' IntString 1);
  print_char (cast' IntChar 97); print_newline ();;

let t' = App (App (Add, Int 1), Int 1)

let () =
  print_string "count_term ";
  print_int (count t'); print_newline ();
  print_newline ();;

let () =
  print t'; print_newline ();;
  


