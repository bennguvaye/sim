open Lacaml.D

type time = float
exception Whoops;;

(******* Functions using the random generator *******)
val rand_float : float -> float

val rand_exp : float -> float

val cumul_sum_l : float list -> float list

val cumul_sum_v : y:Lacaml_float64.vec -> Lacaml_float64.vec -> Lacaml_float64.vec

val rand_choose : 'a list -> float list -> 'a

val multinomial : int -> 'a list -> float list -> 'a list

val print_list : float list -> unit

val print_array : ('a -> unit) -> 'a array -> unit

val array_map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

val array_minus : float array -> float -> float array

val split_array : ('a * 'b) array -> 'a array * 'b array

val map_array_array : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array array

val sc_to_f : ('a -> float Expr.map) -> Expr.symb_calc -> ('a -> float)

(* Moves the first value of x_a to the i-th position *)
val displace_from_0 : 'a array -> int -> 'a array

(* 
 * Returns the index of the first value greater than x in the sorted array a.
 * If x is greater than all the values in a, returns Array.length a
 *)
val pos_to_insert_a : float array -> ?l:int -> ?u:int -> float -> int

(* 
 * Returns the index of the first value greater than x in the sorted vector v 
 * If x is greater than all the values in a, raises Not_found
 *)
val pos_to_insert_v : Lacaml_float64.vec -> ?l:int -> ?u:int -> float -> int
