module Ex = Exercises

let printer (f : 'a -> unit) (lst : 'a list) =
  List.iter f lst;
  print_endline ""
;;

let () =
  (* palindrome *)
  let inp = [ 0; 1; 2; 3; 2; 1; 0 ] in
  Ex.is_palindrome inp |> Printf.printf "is the list a palindrome? %b\n";

  (* flatten *)
  let flattened =
    Ex.flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  in
  printer (fun it -> Printf.printf "%s\n" it) flattened;

  (* compress *)
  let uncompressed = [ 1; 1; 2; 2; 3; 3; 3; 4; 5; 5 ] in
  let compressed = Ex.compress uncompressed in
  printer (fun it -> Printf.printf "%d\n" it) compressed;

  (* pack *)
  let unpacked = [ 1; 1; 2; 2; 3; 4; 4; 4 ] in
  let packed = Ex.pack unpacked in
  printer
    (fun sublist ->
      print_string "new sublist:\n";
      List.iter (fun item -> Printf.printf "%d\n" item) sublist)
    packed;

  (* rlencode *)
  let list_to_encode = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "d"; "d" ] in
  let encoded = Ex.encode list_to_encode in
  printer (fun (count, it) -> Printf.printf "%s:%d " it count) encoded;

  (* rl_encode modified *)
  let encoded_modified = Ex.rle_modded list_to_encode in
  printer
    (fun it ->
      match it with
      | Ex.Single x -> Printf.printf "%s" x
      | Ex.Plural (count, it) -> Printf.printf "%s:%d " it count)
    encoded_modified;

  let decoded =
    Ex.run_len_decode
      [ Ex.Plural (4, "a")
      ; Ex.Single "b"
      ; Ex.Plural (2, "c")
      ; Ex.Plural (2, "a")
      ; Ex.Single "d"
      ; Ex.Plural (4, "e")
      ]
  in
  print_endline "printing decoded list: ";
  printer (fun it -> Printf.printf "%s" it) decoded;

  let duplicated_items = Ex.duplicate_items [ 0; 5; 7; 2; -1; 43 ] in
  print_endline "printing duplicated items: ";
  printer (fun it -> Printf.printf "%d " it) duplicated_items;

  let replicated_items = Ex.replicate_items [ 666; 777; 888 ] 4 in
  print_endline "printing replicated items: ";
  printer (fun it -> Printf.printf "%d " it) replicated_items
;;
