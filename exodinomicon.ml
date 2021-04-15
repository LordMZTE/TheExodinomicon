type entry = { title : string; body : string list }
(** Defines a simple entry with a title and a text body. *)

(** Reads the file at path and returns an entry containing the data. *)
let read_exo path =
  let in_c = open_in path in
  let header = input_line in_c in
  let body = ref [] in
  let rec loop () =
    try
      (* Not sure if using try-with as a control structure is the nice way. *)
      let curr_line = input_line in_c in
      body := curr_line :: !body;
      loop ()
    with End_of_file ->
      close_in_noerr in_c;
      { title = header; body = List.rev !body }
  in
  loop ()

let create_body l =
  let rec aux acc = function
    | [] -> acc
    | x :: xs when x = "" -> aux ("<br>" :: acc) xs
    | x :: xs -> aux (x :: acc) xs
  in
  String.concat " " @@ List.rev (aux [] l)

(** Creates a basic HTML file out of an entry. *)
let exo_to_html e =
  "<html><head><title>" ^ e.title ^ "</title></head><body><h1>" ^ e.title
  ^ "</h1>" ^ String.concat " " e.body ^ "</body></html>"

let () =
  let path =
    try Sys.argv.(1)
    with Invalid_argument _ -> failwith "Please supply a file as argument."
  in

  let exo = read_exo path in
  let html = exo_to_html exo in
  print_endline html
