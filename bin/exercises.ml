let is_palindrome list = list = List.rev list

(*
   a list contains either a single element or a list of nodes that are either
   single elements or themselves lists of nodes
*)
type 'a node = One of 'a | Many of 'a node list

let flatten (lst : 'a node list) : 'a list =
  let rec aux (inp : 'a node list) (res : 'a list) : 'a list =
    match inp with
    | [] ->
        res
    | One it :: t ->
        aux t (it :: res)
    | Many it_list :: t ->
        aux t (aux it_list res)
  in

  aux lst [] |> List.rev

let rec compress (l : 'a list) : 'a list =
  match l with
  (* if a = b then take just the tail and get rid of the a that is a duplicate *)
  (* but if not, then grab that a and include it in next round of compression *)
  | a :: b :: t ->
      if a = b then compress t else a :: compress t
  | smaller ->
      smaller

let pack (lst : 'a list) : 'a list list =
  let rec aux (acc : 'a list list) (curr : 'a list) (remaining : 'a list) :
      'a list list =
    match remaining with
    | [] ->
        List.rev (curr :: acc)
    | x :: xs -> (
      match curr with
      | [] ->
          aux acc [x] xs
      | hd :: _ when hd = x ->
          aux acc (x :: curr) xs
      | _ ->
          aux (curr :: acc) [x] xs )
  in
  aux [] [] lst

let encode (lst : 'a list) : (int * 'a) list =
  let rec aux count acc = function
    | [] ->
        []
    | [x] ->
        (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in

  List.rev (aux 0 [] lst)

let rec _last_two lst =
  match lst with
  | [] | [_] ->
      None
  | [x; y] ->
      Some (x, y)
  | _ :: t ->
      _last_two t

let rec _nth_element (idx : int) (lst : 'a list) : 'a option =
  match lst with
  | [] ->
      None
  | h :: t ->
      if idx = 0 then Some h else _nth_element (idx - 1) t

type 'a rle_item = Single of 'a | Plural of int * 'a

let rle_modded lst =
  let create_tuple count it =
    if count = 1 then Single it else Plural (count, it)
  in

  (* just a reminder: function gets an implicit last arg from the surrounding scope *)
  let rec aux count acc = function
    | [] ->
        []
    | [x] ->
        create_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (create_tuple (count + 1) a :: acc) t
  in

  List.rev (aux 0 [] lst)
