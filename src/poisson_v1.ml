open Lacaml.D
open Basic

module type SYSTEM =
  sig
    (* Should we force the use of a type state ? (Lacaml_int64.vec) *)
    type state
    val to_vec : state -> Lacaml_float64.vec
    val to_list : state -> float list
    val dot : state -> state -> float
    val scal : int -> state -> state
    val sum : state -> state -> state
    val id_l : string list
    (* val in_domain : state -> bool *)
    val index_free : time -> state -> float Expr.map
    val rate_sc_a : Expr.symb_calc array
    val modif_a : state array
    val max_n_to_exhaust : (state -> float) array
  end;;

module type ALGPARAMS =
  sig
    val epsilon : float
    val n_between_print : int
    val n_exp : int
    val prec : float
  end;;

module type INTEGR =
  sig
    type state
    val r_v : Lacaml_float64.vec
    val imp_m : Lacaml_float64.mat
    val compute_rates : time -> state -> unit
    val compute_impacts : time -> state -> unit
    val compute_h : time -> state -> float
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
      map_array_array get_imp_f Sys.rate_sc_a Sys.modif_a

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

    let compute_h t st =
      let compute_eta_i i =
        let upper_exhaust = Sys.max_n_to_exhaust.(i - 1) st in
        let rec f j eta n x x_past_l x_fut_l =
          let x_l = x_past_l @ x_fut_l in
          let x_0 = List.hd x_l in
          let x_tl = List.tl x_l in
          match j <= n_events with
          | false -> (eta, x_past_l @ x_fut_l)
          | true ->
            let upper_rate = Algp.epsilon *. r_v.{j} /. abs_float imp_m.{j, i} in
            let upper = min upper_exhaust upper_rate in
            match upper = infinity with
            | true -> 
              f (j + 1) eta 1 x_0 [x_0] x_tl
            | false ->
              (match x_fut_l with
               | [] -> f j eta n x x_past_l (replenish_q ())
               | y :: tl ->
                 (match float_of_int (n + 1) <= upper with
                  | false ->
                    f (j + 1) x 1 x_0 [x_0] x_tl
                  | true ->
                    (match x +. y > eta with
                     | true -> 
                        f (j + 1) eta 1 x_0 [x_0] x_tl
                     | false -> 
                        f j eta (n + 1) (x +. y) (y :: x_past_l) tl)))
        in
        match q_a.(i - 1) with
        | [] -> raise Whoops ;
        | x_0 :: tl ->
          f 1 infinity 1 x_0 [x_0] tl 
      in 
      for i = 1 to n_events do
        let eta, nq = compute_eta_i i in
        q_a.(i - 1) <- nq ;
        h_v.{i} <- eta /. r_v.{i}
      done ;
      Vec.min h_v

    let update_sys t h st =
      let rec exhaust_measure q eta =
        let rec f n x l =
          match l with
          | [] -> f n x (replenish_q ())
          | y :: tl ->
            (match x +. y <= eta +. Algp.prec with
             | true -> f (n + 1) (x +. y) tl
             | false -> (x, n, l))
        in f 0 0. q
      in
      for i = 1 to n_events do
        let eta = h *. r_v.{i} in
        let tau, n_ev, q = exhaust_measure q_a.(i-1) eta in
        q_a.(i-1) <- (List.hd q -. (eta -. tau)) :: (List.tl q) ;
        n_a.(i-1) <- n_ev
      done ;
      let nt = t +. h in
      let nst = 
       Array.fold_left Sys.sum st
       (array_map2 (fun x -> fun y -> Sys.scal x y) n_a Sys.modif_a)
      in
      (nt, nst)
      (* this last function returns st + sum_i (n_i * modif_i) *)

    let rec to_the_end chan tf t st =
      match t > tf with
      | false ->
          compute_rates t st; compute_impacts t st;
          let h = compute_h t st in
          let t, st = update_sys t h st in
          Csv.output_record chan (csv_line t st) ;
          to_the_end chan tf t st
      | true -> 
          (t, st)

    let simulate st_chan tf x0 =
      let chan = Csv.to_channel st_chan in
      Csv.output_record chan csv_init;
      init_measures () ;
      to_the_end chan tf 0. x0;;
  
    init_measures ()

  end;;

