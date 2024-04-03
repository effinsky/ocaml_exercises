type teacher =
  { name : string
  ; age : int
  }

module OrderedTeacher = struct
  type t = teacher

  let compare t1 t2 =
    match compare t1.name t2.name with
    | 0 -> compare t1.age t2.age
    | c -> c
  ;;
end

module TeacherMap = Map.Make (OrderedTeacher)

let () =
  let teachers = TeacherMap.empty in
  let john = { name = "John"; age = 35 } in

  let alice = { name = "Alice"; age = 28 } in
  let teachers = TeacherMap.add john "Math" teachers in
  let teachers = TeacherMap.add alice "Science" teachers in
  let subject = TeacherMap.find john teachers (* Returns "Math" *) in
  print_endline subject
;;
