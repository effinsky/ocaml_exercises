val is_palindrome : 'a list -> bool

type 'a node = One of 'a | Many of 'a node list

val flatten : 'a node list -> 'a list

val compress : 'a list -> 'a list

val pack : 'a list -> 'a list list

val encode : 'a list -> (int * 'a) list

val _last_two : 'a list -> ('a * 'a) option

val _nth_element : int -> 'a list -> 'a option

type 'a rle_item = Single of 'a | Plural of int * 'a

val rle_modded : 'a list -> 'a rle_item list

val unknown : 'a list -> int list
