
open Lacaml.D
open Basic

(******* Functions using the random generator *******)
module type SYSTEM =
  sig
    type state
    val to_vec : state -> Lacaml_float64.vec
    val to_list : state -> float list
    (** inner product for the system type *)
    val dot : state -> state -> float
    (** outer product for the system type *)
    val scal : int -> state -> state
    (** addition of the system type *)
    val sum : state -> state -> state
    (** names of the dimensions of the system *)
    val id_l : string list
    (** (unused) testing if in the domain of definition of the system *)
    (* val in_domain : state -> bool *)
    val index_free : time -> state -> float Expr.map
    (** symbolic rate functions of the system's events *)
    val rate_sc_a : Expr.symb_calc array
    (** increment to the system state for each event *)
    val modif_a : state array
    (** max number of firings before hitting 0 for each event *)
    val max_n_to_exhaust : (state -> float) array
  end;;

module type ALGPARAMS =
  sig
    (** max relative change a rate can undergo in one jump *)
    val epsilon : float
    (** number of jumps between prints (should become unused) *)
    val n_between_print : int
    (** number of exponential variables computed on each call *)
    val n_exp : int
    (** precision required in the internal time frame *)
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
  (INTEGR with type state = Sys.state)
