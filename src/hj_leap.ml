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
open Basic

(* On pourrait encore au préalable définir un module State avec
 * module type STATE =
 *   sig
 *     type state
 *     val to_vec : state -> Lacaml_float64.vec
 *     val to_list : state -> float list
 *     val dot : state -> state -> float
 *     val sum : state -> state -> state
 *   end;;
 * Puis juste open ça dans System
 *)

module type SYSTEM =
  sig
    type state
    val to_vec : state -> Lacaml_float64.vec
    val to_list : state -> float list
    val dot : state -> state -> float
    val sum : state -> state -> state
    val id_l : string list
    val index_free : time -> state -> float Expr.map
    val rate_sc_a : Expr.symb_calc array
    val min_change_a : float array
    val modif_a : state array
  end;;

module type ALGPARAMS =
  sig
    val step_size : float
    val epsilon : float
    val n_between_print : int
  end;;

module type INTEGR =
  sig
    type state
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

    (* compute f2_jj' second order control (how often should we update h_j *)
    (* rate_sc1 : j, rate_sc2 : j'', modif_st2 : j'', modif_st3 : j' *)
    let get_imp2_f rate_sc1 modif_st3 =
      let partial_f rate_sc2 modif_st2 =
        let msc2 = sc_l_of_st modif_st2 in
        let msc3 = sc_l_of_st modif_st3 in
        let g_sc1 = grad_rate rate_sc1 in
        let gg_sc12 = List.map
                      (fun sc -> grad_rate (Expr.Prod (rate_sc2, sc))) g_sc1 in
        Expr.dot (List.map (fun sc -> Expr.dot sc msc2) gg_sc12) msc3
      in 
      let f = Expr.sum_a (array_map2 partial_f Sys.rate_sc_a Sys.modif_a) in
      (fun t -> sc_to_f (Sys.index_free t) (Expr.keep_reducing f))

    (*** Naming some globals ***)

    (* the a_j, mu_j, f_jj' and f2_jj' arrays *)
    let rate_f_a =
      Array.map
      (fun sc -> 
       fun t -> sc_to_f (Sys.index_free t) sc) Sys.rate_sc_a

    (* Are these in the right order (.(j).(j')) ? Yeah. *)
    let imp_f_a =
      map_array_array get_imp_f Sys.rate_sc_a Sys.modif_a

    let imp2_f_a =
      map_array_array get_imp2_f Sys.rate_sc_a Sys.modif_a

    (* the number of events *)
    let n_events = Array.length Sys.rate_sc_a

    (* let c_v = Vec.make0 n_events *)
    let c_v = Vec.of_array Sys.min_change_a
    let r_v = Vec.make0 n_events
    let cr_v = Vec.make0 n_events
    let imp_v = Vec.make0 n_events
    let avg_imp_v = Vec.make0 n_events
    let sd_imp_v = Vec.make0 n_events
    let tmp_m = Mat.make0 n_events n_events;;

    (*** Functions used in the course of a simulation ***)
    (* after that point we work in numeric (via Lacaml) *)

    (* computes a sequence of impacts (f) for t st and stores the results in v *)
    let compute_impact ~v f t st =
      for k = 1 to n_events do
        v.{k} <- f.(k - 1) t st;
      done ;
      v

    (* Formula for next step size, adapted from Cao 2006
     * r : focus event rate
     * c : minimum rate change for focus event
     * Est-ce que imp_v a un signe ? Est-ce que les impacts s'annulent les uns
     * les autres ? 
     * Est-ce que genre max serait mieux ? Si les impacts s'annulent pas, non.
     *)
    let compute_h r_v imp_v r c =
        let mu = dot imp_v r_v in
        let var = dot (Vec.sqr imp_v) r_v in
        let a = max (Algp.epsilon *. r) c in
        if r < 0. then print_string "r " ; print_float r ; print_string "\n" ;
        min (a /. (abs_float mu)) (a ** 2. /. var)

    (* TODO change name, check indices *)
    (* problem here ? r_v is not updated *)
    (* quoi d'autre diminue h_a ? *)
    let update_leaps r_v j_a h_a t st =
      if h_a.(0) < 0. then (* problem : this never happens *)
        let j = j_a.(0) in
        let r = rate_f_a.(j) t st in
        if r < 0. then
          (print_list (Sys.to_list st) ;
           raise Whoops) ;
        r_v.{j + 1} <- r ;
        let imp_v = compute_impact ~v:imp_v imp_f_a.(j) t st in
        let h = compute_h r_v imp_v r_v.{j + 1} c_v.{j + 1} in
        let k = pos_to_insert_a h_a h in
        if k = 0 then 
          (print_float h_a.(0) ; print_string " " ;
           print_float h ; print_string " " ;
           print_float h_a.(1)) ;
        print_string "a5a\n" ;
        h_a.(0) <- h ;
        let h_a = displace_from_0 h_a (k - 1) in
        let j_a = displace_from_0 j_a (k - 1) in
        (r_v, j_a, h_a)
      else
        (r_v, j_a, h_a)      

    (* TODO check indices *)
    let draw_future r_v j_a h_a t st =
      let r_v, j_a, h_a = update_leaps r_v j_a h_a t st in
      let cr_v = cumul_sum_v ~y:cr_v r_v in
      let lambd = cr_v.{n_events} in
      let tau = rand_exp lambd in
      let next_t = t +. tau in
      let w = rand_float lambd in
      let k = pos_to_insert_v cr_v w in
      let modif = Sys.modif_a.(k - 1) in
      let h_a = array_minus h_a tau in
      let next_st = Sys.sum st modif in
      (r_v, j_a, h_a, next_t, next_st)

    (* TODO change name *)
    (* computes the initial data we need *)
    let init_leaps r_v imp_v st =
      (* initialize the rate vector *)
      for j = 1 to n_events do
        r_v.{j} <- rate_f_a.(j - 1) 0. st
      done ;
      (* initialize the impacts vector *)
      let init_impact ~v j = compute_impact ~v imp_f_a.(j) 0. st in
      let jh_a = Array.init n_events
          (fun j -> (j, compute_h 
                        r_v 
                        (init_impact ~v:imp_v j)
                        r_v.{j + 1} 
                        c_v.{j + 1}))
      in
      Array.sort (fun (j, t) -> fun (k, s) -> compare t s) jh_a ;
      let j_a, h_a = split_array jh_a in
      (r_v, j_a, h_a)

    let rec to_the_end chan tf r_v j_a h_a t st n_since_print =
      match t > tf with
      | false -> 
          let r_v, j_a, h_a, t, st = draw_future r_v j_a h_a t st in
          let n_since_print = 
            if (n_since_print mod Algp.n_between_print) = 0 then
              (Csv.output_record chan (csv_line t st) ; 1)
            else
              n_since_print + 1
          in to_the_end chan tf r_v j_a h_a t st n_since_print
      | true -> 
          (t, st)

    let simulate st_chan tf x0 =
      let chan = Csv.to_channel st_chan in
      Csv.output_record chan csv_init;
      let r_v, j_a, h_a = init_leaps r_v imp_v x0 in
      to_the_end chan tf r_v j_a h_a 0. x0 0;;

  end;;

