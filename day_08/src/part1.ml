(* Day 8: Space Image Format (Part 1) *)

let rec print_array (xs : int array) (m : int ) (n : int) (i : int) : unit =
    if i < m then
        (Array.sub xs i n
         |> Array.to_list
         |> List.map string_of_int
         |> String.concat ""
         |> Printf.fprintf stdout "%s\n";
         print_array xs m n (i + n))
    else
        ()

let calculate (xs : int array) : (int * int) =
    let a : int ref = ref 0 in
    let b : int ref = ref 0 in
    let c : int ref = ref 0 in
    let f : int -> unit = function
        | 0 -> a := !a + 1
        | 1 -> b := !b + 1
        | 2 -> c := !c + 1
        | _ -> () in
    Array.iter f xs;
    (!a, !b * !c)

let iterate (xs : int array) (m : int) (n : int) : int =
    let rec loop (i : int) (threshold : int) (result : int) =
        if i < m then
            let (a, bc) : (int * int) = Array.sub xs i n |> calculate in
            if a < threshold then
                loop (i + n) a bc
            else
                loop (i + n) threshold result
        else
            result in
    loop 0 max_int 0

let () : unit =
    let chan : in_channel = open_in Sys.argv.(1) in
    let n : int =
        input_line chan
        |> String.split_on_char ' '
        |> List.map int_of_string
        |> (function
            | [a; b] -> a * b
            | _ -> exit 1) in
    let s : string = input_line chan in
    close_in chan;
    let m : int = String.length s in
    let xs : int array = Array.make (String.length s) 0 in
    String.iteri (fun i c -> xs.(i) <- Char.escaped c |> int_of_string) s;
    iterate xs m n |> Printf.fprintf stdout "%d\n%!"
