(* TODO check the output *)
(* 
 * Variable naming conventions :
 * (as usual, n, x, y, a, b, l for any ints, floats, arrays, lists)
 * - t for a time, st for a state
 * - A trailing _l, _a, _v, _m denotes (resp) a list, array, vector and matrix
 * - A trailing _f, _sc denotes a function and a Expr.symb_calc
 * - In globals, rates (or r) is used to designate the rates of the system
 *   modifs (or m) for the modifications of the system, 
 *   imp for the impacts (see .tex)
 *   avg for the average, sd for the standard deviation
 *)

open Lacaml.D

type time = float;;
exception Whoops;;

(******* Functions using the random generator *******)
let rec rand_float top =
  match top with
  | 0. -> 0.
  | _  ->
    let u = Random.float top in
    match u with
    | 0. -> rand_float top
    | _  -> u

let rand_exp lambda =
  let u = rand_float 1. in
  -1. *. (log u) /. lambda;;

let rec cumul_sum_l l =
  match l with
  | a :: b :: tl -> a :: (cumul_sum_l ((a +. b) :: tl))   
  | hd :: [] -> l
  | [] -> l (* this case should never happen *);;

let cumul_sum_v ~y v =
  let y = copy ~y v in
  let f i x =
    y.{i} <- x +. y.{i - 1}
  in
  Vec.iteri ~ofsx:2 f y ;
  y

let print_list l =
  List.iter (fun x -> print_float x ; print_string " ") l ;
  print_string "\n"

let print_array a =
  Array.iter (fun x -> print_float x ; print_string " ") a ;
  print_string "\n"

let array_map2 f a1 a2 =
  Array.mapi (fun n -> fun x -> f x a2.(n)) a1

let array_minus a x =
  Array.map (fun y -> y -. x) a

let split_array a = 
  let l1, l2 = List.split (Array.to_list a) in
  (Array.of_list l1, Array.of_list l2)

let matrix_of_map_array_array (f : 'a -> 'b -> 'c) 
                            (a1 : 'a array) (a2 : 'b array) =
  Array.map 
  (fun a ->
  Array.map
  (fun b -> f a b) a2) a1

let sc_to_f map_f sc =
  fun a -> (Expr.eval sc) (map_f a)

(* Moves the first value of x_a to the i-th position *)
let displace_from_0 x_a i =
  let a = Array.concat 
    [(Array.sub x_a 1 i) ; [| x_a.(0) |]]
  in
  Array.blit a 0 x_a 0 (Array.length a) ;
  x_a

(* 
 * Returns the index of the first value greater than x in the sorted array a.
 * If x is greater than all the values in a, returns Array.length a
 *)
(* TODO test *)
(*
let rec pos_to_insert_a a ?(l=0) ?(u=Array.length a - 1) x =
  let rec f l u =
    if l > u then 
      (if (l = 0) && (u = ~-1) then 0 else raise Not_found)
    else    
      (if (l = Array.length a - 1) && (u = Array.length a - 1) then 
         Array.length a
       else
         let m = l + (u - l) / 2 in
         match x > a.(m), x <= a.(m + 1) with
         | false, false -> 
             (print_array a ; invalid_arg "The input array a must be sorted")
         | false, true -> 
             f l (m - 1)
         | true, false -> 
             f (m + 1) u
         | true, true -> 
             m + 1)
  in f l u

(* 
 * Returns the index of the first value greater than x in the sorted vector v 
 * If x is greater than all the values in a, raises Not_found
 *)
let rec pos_to_insert_v v ?(l=1) ?(u=Vec.dim v) x =
  let rec f l u =
    if l > u then 
      (if (l = 1) && (u = 0) then 1
      else raise Not_found)
    else
      let m = l + (u - l) / 2 in
      match x > v.{m}, x <= v.{m + 1} with
      | false, false -> 
          print_float v.{m} ;
          print_string " " ;
          print_float v.{m + 1} ;
          invalid_arg "The input vector v must be sorted"
      | false, true -> f l (m - 1)
      | true, false -> f (m + 1) u
      | true, true -> m + 1
  in f l u;;
*)

module type SYSTEM =
  sig
    type state
    val to_vec : state -> Lacaml_float64.vec
    val to_list : state -> float list
    val dot : state -> state -> float
    val scal : int -> state -> state
    val sum : state -> state -> state
    val id_l : string list
    val in_domain : state -> bool
    val index_free : time -> state -> float Expr.map
    val rate_sc_a : Expr.symb_calc array
    val min_change_a : float array
    val modif_a : state array
  end;;

module type ALGPARAMS =
  sig
    val epsilon : float
    val n_between_print : int
    val n_exp : int
  end;;

module type INTEGR =
  sig
    type state
    val r_v : Lacaml_float64.vec
    val imp_m : Lacaml_float64.mat
    val compute_rates : time -> state -> unit
    val compute_impacts : time -> state -> unit
    val compute_h_ij : int -> int -> time -> state -> float
    val update_sys : time -> time -> state -> time * state
    val simulate :
          out_channel ->
          float ->
          state ->
          (time * state)
  end;;

module Integrator (Sys : SYSTEM) (Algp : ALGPARAMS) : 
  (INTEGR with type state = Sys.state) =
  struct
    type state = Sys.state
    (*** System dependent operations ***)
    let grad_rate sc = Expr.grad Sys.id_l sc
    let sc_l_of_st st = List.map (fun x -> Expr.Cons x) (Sys.to_list st)

    let csv_init = 
      "t" :: Sys.id_l

    let csv_line t st =
      (string_of_float t) :: (List.map string_of_float (Sys.to_list st));;
    
    (*** Symbolic computations ***)

    (* we compute symbolically "f_jj'" (see many papers on tau-leap) 
     * in the hope that we can simplify a lot of zeros 
     * (typically, modifications only affect certain compartments) *)
    (* Do the expressions get simplified enough ? *)
    (* rate_sc : j, modif_st : j' *)
    let get_imp_f rate_sc modif_st =
      (* modification of each component of the state as a symbolic list *)
      let msc = sc_l_of_st modif_st in
      (* the gradient of the rate function as a list *)
      let g_sc = grad_rate rate_sc in
      (* the symbolic scalar product of the two lists *)
      let f = Expr.dot g_sc msc in
      (fun t -> 
       sc_to_f (Sys.index_free t) (Expr.keep_reducing f))

    (*** Naming some globals ***)

    (* the a_j, mu_j, f_jj' and f2_jj' arrays *)
    let rate_f_a =
      Array.map
      (fun sc -> 
       fun t -> sc_to_f (Sys.index_free t) sc) Sys.rate_sc_a

    (* Are these in the right order (.(j).(j')) ? Yeah. *)
    let imp_f_a =
      matrix_of_map_array_array get_imp_f Sys.rate_sc_a Sys.modif_a

    (* the number of events *)
    let n_events = Array.length Sys.rate_sc_a

    let r_v = Vec.make0 n_events
    let imp_m = Mat.make0 n_events n_events
    let eta_i_v = Vec.make0 n_events
    let h_v = Vec.make0 n_events
    let q_a = Array.make n_events []
    let n_a = Array.make n_events 0 ;;

    (*** Functions used in the course of a simulation ***)
    (* after that point we work in numeric (via Lacaml) *)

    let compute_rates t st =
      for k = 1 to n_events do
        r_v.{k} <- rate_f_a.(k - 1) t st
      done

    (* computes a sequence of impacts (f) for t st and stores the results in v *)
    let compute_impacts t st =
      for k = 1 to n_events do
        for l = 1 to n_events do
          imp_m.{k,l} <- imp_f_a.(k - 1).(l - 1) t st;
        done ;
      done

    let replenish_q () = 
      Array.to_list (Array.init Algp.n_exp (fun n -> rand_exp 1.))

    (* computes the initial data we need *)
    let init_measures () =
      for i = 0 to n_events - 1 do  
        q_a.(i) <- replenish_q ()
      done

    let compute_h_ij i j t st =
      let compute_eta_ij q r imp =
        let upper = Algp.epsilon *. r /. abs_float imp in
        let rec f x x_past_l x_fut_l =
          match x_fut_l with
          | [] -> print_string "a" ; f x x_past_l (replenish_q ())
          | y :: tl ->
            (match x +. y < upper with
             | true -> f (x +. y) (y :: x_past_l) tl
             | false -> (x, x_past_l, x_fut_l))
        in f 0. q [] (* ou f tau_i q  ? *)
      in 
      let eta_ij, q_i, nq_i = compute_eta_ij q_a.(i-1) r_v.{j} imp_m.{i,j} in
      eta_ij /. r_v.{i}
  
    let update_sys t h st =
      let rec exhaust_measure q eta =
        let rec f x n l =
          match l with
          | [] -> f x n (replenish_q ())
          | y :: tl ->
            (match x +. y < eta with
             | true -> f (x +. y) (n + 1) tl
             | false -> (x, n, l))
        in f 0. 0 q
      in
      for i = 1 to n_events do
        let eta = h /. r_v.{i} in
        let tau, n_ev, q = exhaust_measure q_a.(i-1) eta in
        q_a.(i-1) <- (List.hd q -. (eta -. tau)) :: (List.tl q) ;
        n_a.(i-1) <- n_ev
      done ;
      (t +. h, 
       Array.fold_left Sys.sum st
       (array_map2 (fun x -> fun y -> Sys.scal x y) n_a Sys.modif_a))
      (* this last function returns st + sum_i (n_i * modif_i) *)

    let rec to_the_end chan tf t st n_since_print =
      match t > tf with
      | false ->
          compute_rates t st; compute_impacts t st;
          let h_v = compute_h_v t st in
          let t, st = update_sys t h_v st in
          let n_since_print = 
            if (n_since_print mod Algp.n_between_print) = 0 then
              (Csv.output_record chan (csv_line t st) ; 1)
            else
              n_since_print + 1
          in to_the_end chan tf t st n_since_print
      | true -> 
          (t, st)

    let simulate st_chan tf x0 =
      let chan = Csv.to_channel st_chan in
      Csv.output_record chan csv_init;
      init_measures () ;
      to_the_end chan tf 0. x0 0;;
  
    init_measures ()

  end;;

