open Lacaml.D
open Basic

let b = 10.
let d = 1.
let bet = 1.2
let del = 1.1;;

module System : (Hj_leap.SYSTEM with type state = int * int) = 
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
  
    let sum st1 st2 =
      let n1, m1 = st1
      and n2, m2 = st2
      in (n1 + n2, m1 + m2)
    
    let id_l = ["N" ; "M"]
    
    let index_free (t : time) (st : state) =
      let n, m = st in
      Expr.map_of_assoc_list [("N", float_of_int n) ; ("M", float_of_int m)]
  
    let rate_sc_a = 
      [| Expr.Prod (Expr.Cons b, Expr.Id "N") ;
         Expr.Prod (Expr.Cons bet, Expr.Id "N") ;
         Expr.Prod (Expr.Cons d, Expr.Prod (Expr.Id "N", Expr.Id "M")) ;
         Expr.Prod (Expr.Cons del, Expr.Id "M") |]
      
    let min_change_a = 
      [| b ;
         bet ; 
         d ;
         del |]
          
    let modif_a = 
      [|   (1, 0) ;
           (0, 1) ;
         (~-1, 0) ;
         (0, ~-1) |]
  end

module Algp =
  struct
    let step_size = 0.1
    let epsilon = 0.01
    let n_between_print = 100
  end

module Gen = Hj_leap.Integrator (System) (Algp);;

Gen.simulate stdout 10000. (1, 0);;
