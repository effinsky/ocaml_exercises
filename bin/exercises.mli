val is_palindrome : 'a list -> bool

type 'a list_node =
  | One of 'a
  | Many of 'a list_node list

val flatten : 'a list_node list -> 'a list
val compress : 'a list -> 'a list
val pack : 'a list -> 'a list list
val encode : 'a list -> (int * 'a) list
val last_two : 'a list -> ('a * 'a) option
val nth_element : int -> 'a list -> 'a option

type 'a rle_item =
  | Single of 'a
  | Plural of int * 'a

val rle_modded : 'a list -> 'a rle_item list
val run_len_decode : 'a rle_item list -> 'a list
val duplicate_items : 'a list -> 'a list
val replicate_items : 'a list -> int -> 'a list
