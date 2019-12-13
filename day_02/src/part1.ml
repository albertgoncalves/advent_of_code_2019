let rec pretty_print : int list -> unit = function
    | a :: b :: c :: d :: xs ->
        Printf.fprintf stdout "%d\t%d\t%d\t%d\n" a b c d;
        pretty_print xs
    | a :: b :: c :: [] -> Printf.fprintf stdout "%d\t%d\t%d\n" a b c
    | a :: b :: [] -> Printf.fprintf stdout "%d\t%d\n" a b
    | a :: [] -> Printf.fprintf stdout "%d\n" a
    | [] -> flush stdout

let () : unit =
    input_line stdin
    |> String.split_on_char ','
    |> List.map int_of_string
    |> pretty_print
