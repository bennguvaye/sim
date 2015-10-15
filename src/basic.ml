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

let rand_choose c p =
  let ss = cumul_sum_l p in
  let u = rand_float 1. in
  let rec f l1 l2 =
    match l1, l2 with
    | (_ :: _, []) | ([], _ :: _) -> 
        failwith "the two lists have different lengths"
    | [], [] -> 
        failwith "empty arguments"
    | hd1 :: tl1, hd2 :: tl2 ->
        (match u < hd2 with
         | true -> hd1 
         | false -> f tl1 tl2)
  in
  f c ss

let multinomial n c p =
  let rec f n l =
    match n with (* problem if n < 0 *)
    | 0 -> l
    | _ -> f (n - 1) (rand_choose c p :: l)
  in f n []

let print_list l =
  List.iter (fun x -> print_float x ; print_string " ") l ;
  print_string "\n"

let print_array print_fun a =
  Array.iter (fun x -> print_fun x ; print_string " ") a ;
  print_string "\n"

let array_map2 f a1 a2 =
  Array.mapi (fun n -> fun x -> f x a2.(n)) a1

let array_minus a x =
  Array.map (fun y -> y -. x) a

let split_array a = 
  let l1, l2 = List.split (Array.to_list a) in
  (Array.of_list l1, Array.of_list l2)

let map_array_array (f : 'a -> 'b -> 'c) 
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
             (print_array print_float a ; invalid_arg "The input array a must be sorted")
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
