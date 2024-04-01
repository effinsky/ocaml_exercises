open Exercises

let () =
  let inp = [ 0; 1; 2; 3; 2; 1; 0 ] in
  is_palindrome inp |> Printf.printf "is the list a palindrome? %b\n";

  let arr = [| 0; 1; 2; 3; 4 |] in
  Array.set arr 3 666;

  print_endline "printing array";
  Array.iter (fun it -> print_int it) arr;

  print_endline "";

  let flattened =
    flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  in
  List.iter (fun it -> Printf.printf "%s\n" it) flattened;

  let uncompressed = [ 1; 1; 2; 2; 3; 3; 3; 4; 5; 5 ] in
  let compressed = compress uncompressed in
  List.iter (fun it -> Printf.printf "%d\n" it) compressed;

  let unpacked = [ 1; 1; 2; 2; 3; 4; 4; 4 ] in
  let packed = pack unpacked in
  List.iter
    (fun sublist ->
      print_string "new sublist:\n";
      List.iter (fun item -> Printf.printf "%d\n" item) sublist)
    packed;

  let list_to_encode = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "d"; "d" ] in
  let encoded = encode list_to_encode in
  List.iter (fun (count, it) -> Printf.printf "%d : %s" count it) encoded
;;
