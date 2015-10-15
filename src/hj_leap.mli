(** Simulations of Markov processes via a modified tau-leaping procedure, 
    hereafter called the hj-leaping procedure.
    
    Each reaction of the system get its own "tau", giving the time-intervals
    between the updates of the rate-value.

    It depends on expr for symbolic manipulations. *)

open Basic

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
  (INTEGR with type state = Sys.state)
