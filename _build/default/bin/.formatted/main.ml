let read_file filename =
  try
    let input_channel = open_in filename in
    try
      while true do
        input_line input_channel |> print_endline
      done
    with End_of_file -> close_in input_channel
  with Sys_error err -> Printf.printf "Error: %s\n" err
;;

read_file "example.txt"

let write_to_file filename =
  try
    let output_channel = open_out filename in
    close_out output_channel
  with Sys_error err -> Printf.printf "Error: %s\n" err
;;

write_to_file "output.txt";
write_to_file "output.txt"
