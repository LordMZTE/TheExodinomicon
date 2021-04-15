
(** Defines a simple entry with a title and a text body. *)
type entry =
  { title : string;
    body : string list;
  }

(** Reads the file at path and returns an entry containing the data. *)
let read_exo path =
  let in_c = open_in path in
  let header = input_line in_c in
  let body = ref [] in
  let rec loop ()  =
    try      (** Not sure if using try-with as a control structure is the nice way. *)
      let curr_line = input_line in_c in
      body :=  curr_line :: !body;
      loop ()
    with
      End_of_file ->
      begin
        close_in_noerr in_c;
        { title = header;
          body = List.rev !body;
        }
      end in
  loop ()

let create_body l =
  let rec aux acc = function
    |[] -> acc
    |x :: xs when x = "" -> aux ("<br>" :: acc) xs
    |x :: xs -> aux (x :: acc) xs in
  string_list_to_string @@ List.rev (aux [] l)

let rec string_list_to_string = function
  |[] -> ""
  |x :: xs -> x ^ " " ^ string_list_to_string xs

(** Creates a basic HTML file out of an entry. *)
let exo_to_html e =
  "<html>
   <head> 
   <title>"
  ^ e.title ^
    "</title>
     </head>
     <body>
     <h1>"
    ^ e.title ^
      "</h1>"
      ^ string_list_to_string e.body ^
        "</body>
         </html>"
