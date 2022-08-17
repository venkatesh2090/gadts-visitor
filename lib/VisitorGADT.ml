type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

let i = Int 1
let add = Add
let app = App (App (Add,  Int 1), Int 1)

class print_visitor = object(self)
  method visit_Int : int -> unit =
    fun n -> print_string "visit_Int "; self#visit_int n

  method visit_int : int -> unit =
    fun n -> Printf.printf "visit_int %d " n

  method visit_Add : unit -> unit =
    fun () -> print_string "visit_Add "

  method visit_App : 'a 'b. ('b -> 'a) term -> 'b term -> unit =
    fun t0 t1 ->
    print_string "visit_App ";
    let _ = self#visit_term t0 in
    let _ = self#visit_term t1 in
    ()

  method visit_term : type a. a term -> unit =
    function
    | Int n -> self#visit_Int n 
    | Add -> self#visit_Add ()
    | App (f, x) -> self#visit_App f x  
end

let print : 'a. 'a term -> unit = function t ->
  let v = new print_visitor in
  v#visit_term t

class count_visitor = object(self)
  val mutable count : int = 0

  method visit_Int : int -> unit =
    fun n -> count <- count + 1; self#visit_int n

  method visit_int : int -> unit =
    fun _ -> 
      count <- count + 1;

  method visit_Add : unit -> unit =
    fun () -> count <- count + 1; ()

  method visit_App : 'a 'b. ('b -> 'a) term -> 'b term -> unit =
    fun t0 t1 ->
    let _ = self#visit_term t0 in
    let _ = self#visit_term t1 in
    ()

  method visit_term : type a. a term -> unit =
    function
    | Int n -> self#visit_Int n 
    | Add -> self#visit_Add ()
    | App (f, x) -> self#visit_App f x  

  method get_count : unit -> int = function () -> count
end

let count : 'a. 'a term -> int =
  fun t ->
    let v = new count_visitor in
    v#visit_term t;
    v#get_count () 


class map_visitor = object(self)
  method visit_int (n : int) : int = n * 2

  method visit_Int : int -> int term =
    fun n -> Int n

  method visit_Add : unit -> (int -> int -> int) term =
    fun () -> Add

  method visit_App : type a b. (b -> a) term -> b term -> a term =
    fun f x ->
    let rf : (b -> a) term  = self#visit_term f in
    let rx : b term = self#visit_term x in
    App (rf, rx)


  method visit_term : type a. a term -> a term = function
    | Int n -> self#visit_Int n
    | Add -> self#visit_Add ()
    | App (f,x) -> self#visit_App f x
end

let map : 'a. 'a term -> 'a term =
  fun t ->
    (new map_visitor)#visit_term t




