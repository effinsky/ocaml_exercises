module Ex = Exercises
module StringMap = Map.Make (String)

let printer (f : 'a -> unit) (lst : 'a list) =
  List.iter f lst ; print_endline ""
;;

let pr_int it = Printf.printf "%d " it

let pr_str it = Printf.printf "%s " it

let pr_int_str (count, it) = Printf.printf "%s:%d " it count

let () =
  (* palindrome *)
  let inp = [0; 1; 2; 3; 2; 1; 0] in
  Ex.is_palindrome inp |> Printf.printf "is the list a palindrome? %b\n" ;

  (* flatten *)
  let flattened =
    Ex.flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]
  in
  printer pr_str flattened ;

  (* compress *)
  let uncompressed = [1; 1; 2; 2; 3; 3; 3; 4; 5; 5] in
  let compressed = Ex.compress uncompressed in
  printer pr_int compressed ;

  (* pack *)
  let unpacked = [1; 1; 2; 2; 3; 4; 4; 4] in
  let packed = Ex.pack unpacked in
  printer
    (fun sublist ->
      print_string "new sublist:\n" ;
      List.iter pr_int sublist )
    packed ;

  (* rlencode *)
  let list_to_encode = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "d"; "d"] in
  let encoded = Ex.encode list_to_encode in
  printer pr_int_str encoded ;

  (* rl_encode modified *)
  let encoded_modified = Ex.rle_modded list_to_encode in
  printer
    (fun it ->
      match it with
      | Ex.Single x ->
          Printf.printf "%s" x
      | Ex.Plural (count, it) ->
          Printf.printf "%s:%d " it count )
    encoded_modified ;

  let decoded =
    Ex.run_len_decode
      [ Ex.Plural (4, "a")
      ; Ex.Single "b"
      ; Ex.Plural (2, "c")
      ; Ex.Plural (2, "a")
      ; Ex.Single "d"
      ; Ex.Plural (4, "e") ]
  in
  print_endline "decoded list: " ;
  printer pr_str decoded ;

  let duplicated_items = Ex.duplicate_items [0; 5; 7; 2; -1; 43] in
  print_endline "duplicated items: " ;
  printer pr_int duplicated_items ;

  let replicated_items = Ex.replicate_items [666; 777; 888] 4 in
  print_endline "replicated items: " ;
  printer pr_int replicated_items ;

  let list_to_remove_nth_from = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let removed_nth = Ex.remove_nth list_to_remove_nth_from 3 in
  print_endline "list with nth items removed: " ;
  printer pr_int removed_nth ;

  print_endline "----------------------------------------------" ;

  (* split list *)
  let list_to_split = [6; 3; 4; 6; 2; 2; 4; 4; 2] in
  print_endline "list_to_split" ;
  printer pr_int list_to_split ;
  let l_1, l_2 = Ex.split_list_at list_to_split 5 in
  print_endline "split list" ;
  printer pr_int l_1 ;
  printer pr_int l_2 ;

  print_endline "----------------------------------------------" ;

  (* slice from list *)
  let list_to_slice = [8; 7; 6; 5; 4; 3; 2; 1; 0] in
  let sliced = Ex.Slice_from_to_proper.slice list_to_slice 3 5 in
  print_endline "sliced list" ;
  printer pr_int sliced ;

  print_endline "----------------------------------------------" ;

  (* remove nth element from list *)
  let nth_to_remove = ["something"; "nothing"; "everything"; "anything"] in
  let with_2nd_removed = Ex.remove_nth nth_to_remove 2 in
  printer pr_str with_2nd_removed ;

  print_endline "----------------------------------------------" ;

  (* rotate list n positions to the left *)
  let l_to_rotate = [1; 2; 3; 4; 5; 6] in
  let pos = 4 in
  let rotated_left = Ex.rotate_left l_to_rotate pos in
  Printf.printf "list to rotate by %d\n" pos ;
  printer pr_int l_to_rotate ;
  print_endline "rotated list" ;
  printer pr_int rotated_left ;

  (* insert an item at a given position into a list *)
  let l_to_insert_to = ["alpha"; "beta"; "delta"] in
  let pos = 2 in
  let l_inserted = Ex.insert_at "gamma" pos l_to_insert_to in

  print_endline "printing list before insertion" ;
  printer pr_str l_to_insert_to ;
  Printf.printf "printing list with element inserted at position %d\n" pos ;
  (* will unwrap option for this case for brevity *)
  l_inserted |> Option.get |> printer pr_str
;;
