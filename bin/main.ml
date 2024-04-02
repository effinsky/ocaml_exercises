let printer (f : 'a -> unit) (lst : 'a list) = List.iter f lst

let () =
  (* palindrome *)
  let inp = [ 0; 1; 2; 3; 2; 1; 0 ] in
  Exercises.is_palindrome inp |> Printf.printf "is the list a palindrome? %b\n";

  (* flatten *)
  let flattened =
    Exercises.flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  in
  printer (fun it -> Printf.printf "%s\n" it) flattened;

  (* compress *)
  let uncompressed = [ 1; 1; 2; 2; 3; 3; 3; 4; 5; 5 ] in
  let compressed = Exercises.compress uncompressed in
  printer (fun it -> Printf.printf "%d\n" it) compressed;

  (* pack *)
  let unpacked = [ 1; 1; 2; 2; 3; 4; 4; 4 ] in
  let packed = Exercises.pack unpacked in
  printer
    (fun sublist ->
      print_string "new sublist:\n";
      List.iter (fun item -> Printf.printf "%d\n" item) sublist)
    packed;

  (* rlencode *)
  let list_to_encode = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "d"; "d" ] in
  let encoded = Exercises.encode list_to_encode in
  printer (fun (count, it) -> Printf.printf "%d : %s" count it) encoded;

  (* rl_encode modified *)
  let encoded_modified = Exercises.rle_modded list_to_encode in
  printer
    (fun it ->
      match it with
      | Exercises.Single x -> Printf.printf "%s" x
      | Exercises.Plural (count, it) -> Printf.printf "%d : %s" count it)
    encoded_modified
;;

