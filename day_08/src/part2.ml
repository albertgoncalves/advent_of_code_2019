(* Day 8: Space Image Format (Part 2) *)

let rec print_array (m : int ) (n : int) (i : int) (xs : int array) : unit =
    if i < m then begin
        Array.sub xs i n
        |> Array.to_list
        |> List.map begin function
            | 0 -> "_"
            | 1 -> "#"
            | 2 -> " "
            | _ -> exit 1
        end
        |> String.concat ""
        |> Printf.fprintf stdout "%s\n";
        print_array m n (i + n) xs
    end else
        ()

let rec calculate (xs : int array) (m : int) (n : int) (i : int) : int =
    if i < m then
        match xs.(i) with
            | 0 | 1 -> xs.(i)
            | 2 -> calculate xs m n (i + n)
            | _ -> exit 1
    else
        xs.(i - n)

let () : unit =
    let chan : in_channel = open_in Sys.argv.(1) in
    let (width, height) : (int * int) =
        input_line chan
        |> String.split_on_char ' '
        |> List.map int_of_string
        |> begin function
            | [a; b] -> (a, b)
            | _ -> exit 1
        end in
    let n : int = width * height in
    let s : string = input_line chan in
    close_in chan;
    let m : int = String.length s in
    let xs : int array = Array.make (String.length s) 0 in
    String.iteri (fun i c -> xs.(i) <- Char.escaped c |> int_of_string) s;
    Array.mapi (fun i _ -> calculate xs m n i) (Array.make n 0)
    |> print_array n width 0;
    flush stdout
