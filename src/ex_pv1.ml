open Lacaml.D
open Basic

let b = 50.
let d = 1.
let bet = 0.12
let del = 1.1;;

module System : (Poisson_v1.SYSTEM with type state = int * int) = 
  struct
    type state = int * int

    let to_vec st =
      let n, m = st in
      let v = Vec.make0 2 in
      v.{1} <- float_of_int n ; v.{2} <- float_of_int m ;
      v

    let to_list (st : state) =
      let n, m = st in
      float_of_int n :: float_of_int m :: []

    let dot st1 st2 =
      let n1, m1 = st1
      and n2, m2 = st2
      in float_of_int (n1 * n2 + m1 * m2)

    let scal k st =
      let n, m = st in
      (k * n, k * m)
  
    let sum st1 st2 =
      let n1, m1 = st1
      and n2, m2 = st2
      in (n1 + n2, m1 + m2)
    
    let id_l = ["N" ; "M"]

    (*
     * let in_domain st =
     *   let n, m = st in
     *   if (n >= 0) && (m >= 0) then true else false
     *)

    let index_free (t : time) (st : state) =
      let n, m = st in
      Expr.map_of_assoc_list [("N", float_of_int n) ; ("M", float_of_int m)]
  
    let rate_sc_a = 
      [| Expr.Prod (Expr.Cons b, Expr.Id "N") ;
         Expr.Prod (Expr.Cons bet, Expr.Id "N") ;
         Expr.Prod (Expr.Cons d, Expr.Prod (Expr.Id "N", Expr.Id "M")) ;
         Expr.Prod (Expr.Cons del, Expr.Id "M") |]
      
    let modif_a = 
      [|   (1, 0) ;
           (0, 1) ;
         (~-1, 0) ;
         (0, ~-1) |]

    (* for reference for more complicated systems (not needed here) *)
    let max_n_to_exhaust_fun modif st =
      let nu, mu = modif in
      let n, m = st in
      if (nu < 0) && (mu < 0) then 
        min (~-. (float_of_int n) /. (float_of_int nu)) 
            (~-. (float_of_int m) /. (float_of_int mu))
      else 
        if (nu < 0) then
          (~-. (float_of_int n) /. (float_of_int nu))
        else
          if (mu < 0) then
            (~-. (float_of_int m) /. (float_of_int mu))
          else
            infinity

    let max_n_to_exhaust =
      [| (fun (n, m) -> infinity) ;
         (fun (n, m) -> infinity) ;
         (fun (n, m) -> float_of_int n) ;
         (fun (n, m) -> float_of_int m) |]

  end;;

module Algp =
  struct
    let n_exp = 100
    let epsilon = 0.1
    let prec = 1e-8
    let n_between_print = 1
  end;;

module Gen = Poisson_v1.Integrator (System) (Algp);;

Gen.simulate stdout 10. (1, 0);;
