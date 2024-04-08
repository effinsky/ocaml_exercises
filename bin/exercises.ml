let is_palindrome list = list = List.rev list

(*
   a list contains either a single element or a list of nodes that are either
   single elements or themselves lists of nodes
*)
type 'a list_node =
  | One of 'a
  | Many of 'a list_node list

let flatten (lst : 'a list_node list) : 'a list =
  let rec aux (inp : 'a list_node list) (res : 'a list) : 'a list =
    match inp with
    | [] -> res
    | One it :: t -> aux t (it :: res)
    | Many it_list :: t -> aux t (aux it_list res)
  in

  aux lst [] |> List.rev
;;

let rec compress (l : 'a list) : 'a list =
  match l with
  (* if a = b then take just the tail and get rid of the a that is a duplicate *)
  (* but if not, then grab that a and include it in next round of compression *)
  | a :: b :: t -> if a = b then compress t else a :: compress t
  | smaller -> smaller
;;

let pack (lst : 'a list) : 'a list list =
  let rec aux (acc : 'a list list) (curr : 'a list) (remaining : 'a list) : 'a list list =
    match remaining with
    | [] -> List.rev (curr :: acc)
    | x :: xs ->
      (match curr with
       | [] -> aux acc [ x ] xs
       | hd :: _ when hd = x -> aux acc (x :: curr) xs
       | _ -> aux (curr :: acc) [ x ] xs)
  in
  aux [] [] lst
;;

let encode (lst : 'a list) : (int * 'a) list =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in

  List.rev (aux 0 [] lst)
;;

let rec last_two lst =
  match lst with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t
;;

let rec nth_element (idx : int) (lst : 'a list) : 'a option =
  match lst with
  | [] -> None
  | h :: t -> if idx = 0 then Some h else nth_element (idx - 1) t
;;

type 'a rle_item =
  | Single of 'a
  | Plural of int * 'a

let rle_modded lst =
  let mk_tuple count it = if count = 1 then Single it else Plural (count, it) in

  (* just a reminder: function gets an implicit last arg from the surrounding scope *)
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> mk_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 (mk_tuple (count + 1) a :: acc) t
  in

  List.rev (aux 0 [] lst)
;;

(* rldecode*)
let run_len_decode (lst : 'a rle_item list) : 'a list =
  let unpack_items (x : 'a rle_item) =
    match x with
    | Single x -> [ x ]
    | Plural (count, x) -> List.init count (fun _ -> x)
  in

  let rec aux (acc : 'a list) = function
    | [] -> acc
    | h :: t ->
      let items = unpack_items h in
      aux (acc @ items) t
  in

  aux [] lst
;;

let duplicate_items l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in
  aux [] l |> List.rev
;;

let replicate_items l count =
  let create_partial count it = List.init count (fun _ -> it) in

  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (acc @ create_partial count h) t
  in

  aux [] l
;;

(* Split a list into two parts; the length of the first part is given. *)
let split_list_at (lst : 'a list) (at : int) : 'a list * 'a list =
  let rec aux idx (acc : 'a list * 'a list) = function
    | [] -> acc
    | h :: t ->
      let fst, snd = acc in
      let inc_idx = idx + 1 in
      if idx < at then aux inc_idx (h :: fst, snd) t else aux inc_idx (fst, h :: snd) t
  in

  let l1, l2 = aux 0 ([], []) lst in
  List.rev l1, List.rev l2
;;

module Slice_from_to_proper = struct
  (* I will contrain one of the type params to be a list of the other type param
     type normally we could have the fold_until be more flexible in terms of type
     of acc accepted, but it's scoped here to be an unexported helper *)
  let rec fold_until (f : 'a list -> 'a -> 'a list) (acc : 'a list) n = function
    | [] -> acc, []
    | h :: t as l -> if n = 0 then acc, l else fold_until f (f acc h) (n - 1) t
  ;;

  let slice lst i j =
    (* get list to take from by dropping elements / folding until *)
    let _, lst_sans_dropped = fold_until (fun _ _ -> []) [] i lst in
    let taken, _ = fold_until (fun acc h -> h :: acc) [] (j - i + 1) lst_sans_dropped in
    List.rev taken
  ;;
end

let slice_from_to_naive l i1 i2 =
  (* drop n first elements *)
  let rec drop n = function
    | [] -> []
    | _ :: t -> if n = 0 then t else drop (n - 1) t
  in
  (* grab n first items from the list with the first n items dropped *)
  let rec take n = function
    | [] -> []
    | h :: t ->
      (* problem is it's not tail recursive and there's some duplication
         of structure of the take helper, so this might blow the stack *)
      if n = 0 then [] else h :: take (n - 1) t
  in
  (* much more readable left to right like that *)
  l |> drop (i1 - 1) |> take (i2 - i1 + 1)
;;

(* recurse over orig list while returning an accumnlator; skip of the the nth
   element *)
let remove_nth (l : 'a list) (n : int) : 'a list =
  let rec aux (acc : 'a list) (curr_idx : int) = function
    | [] -> acc
    | h :: t ->
      if curr_idx = n then aux acc (curr_idx + 1) t else aux (h :: acc) (curr_idx + 1) t
  in

  List.rev @@ aux [] 0 l
;;

let rotate_left (lst : 'a list) (n : int) =
  (* ensure n is within list bounds *)
  let offset = n mod List.length lst in
  (* reuse our split fun from above *)
  let fst, snd = split_list_at lst offset in

  snd @ fst
;;

let insert_at item at lst =
  if at < 0 || at > List.length lst
  then None
  else (
    let rec aux idx acc = function
      | [] -> List.rev acc
      | h :: t ->
        (* item prepended first, then head *)
        if idx = at then aux idx (h :: item :: acc) t else aux (idx + 1) (h :: acc) t
    in

    Some (aux 0 [] lst))
;;
