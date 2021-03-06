(* Day 2: 1202 Program Alarm (Part 1) | Answer: 9706670 *)

let calculate (xs : int array) : int array =
    xs.(1) <- 12;
    xs.(2) <- 2;
    let rec loop n i =
        if (n <= i) || (xs.(i) = 99) then
            xs
        else
            let j : int = xs.(i + 1) in
            let k : int = xs.(i + 2) in
            let l : int = xs.(i + 3) in
            (
                match xs.(i) with
                    | 1 -> xs.(l) <- xs.(j) + xs.(k)
                    | 2 -> xs.(l) <- xs.(j) * xs.(k)
                    | _ -> ()
            );
            loop n (i + 4) in
    loop (Array.length xs) 0

let rec pretty_print : int list -> unit = function
    | a :: b :: c :: d :: xs ->
        Printf.fprintf stdout "%d\t%d\t%d\t%d\n" a b c d;
        pretty_print xs
    | a :: b :: c :: [] -> Printf.fprintf stdout "%d\t%d\t%d\n" a b c
    | a :: b :: [] -> Printf.fprintf stdout "%d\t%d\n" a b
    | a :: [] -> Printf.fprintf stdout "%d\n" a
    | [] -> ()

let () : unit =
    at_exit (fun () : unit -> flush stdout);
    input_line stdin
    |> String.split_on_char ','
    |> List.map int_of_string
    |> Array.of_list
    |> calculate
    |> Array.to_list
    |> pretty_print
