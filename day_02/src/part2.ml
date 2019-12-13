let calculate (xs : int array) (a : int) (b : int) : int array =
    xs.(1) <- a;
    xs.(2) <- b;
    let rec loop (n : int) (i : int) : unit =
        if (n <= i) || (xs.(i) = 99) then
            ()
        else
            let j : int = xs.(i + 1) in
            let k : int = xs.(i + 2) in
            let l : int = xs.(i + 3) in
            let _ : unit = match xs.(i) with
                | 1 -> xs.(l) <- xs.(j) + xs.(k)
                | 2 -> xs.(l) <- xs.(j) * xs.(k)
                | _ -> () in
            loop n (i + 4) in
    loop (Array.length xs) 0;
    xs

let iterate (xs : int array) : int option =
    let rec loop (a : int) (b : int) =
        if (calculate (Array.copy xs) a b).(0) = 19690720 then
            Some ((100 * a) + b)
        else if (a = 99) && (b = 99) then
            None
        else if a < 99 then
            loop (a + 1) b
        else if b < 99 then
            loop 0 (b + 1)
        else
            None in
    loop 0 0

let maybe_print : int option -> unit = function
    | Some x -> Printf.fprintf stdout "\t%d\n%!" x
    | None -> ()

let () : unit =
    input_line stdin
    |> String.split_on_char ','
    |> List.map int_of_string
    |> Array.of_list
    |> iterate
    |> maybe_print
