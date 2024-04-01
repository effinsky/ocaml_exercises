val is_palindrome : 'a list -> bool
val encode : 'a list -> (int * 'a) list
val pack : 'a list -> 'a list list
val compress : 'a list -> 'a list

type 'a node =
  | One of 'a
  | Many of 'a node list

val flatten : 'a node list -> 'a list
