(* Day 9: Sensor Boost (Part 1) | Answer: 4234906522
                       (Part 2) | Answer: 60962 *)

type program = {
    mutable xs : int array;
    mutable i : int;
    mutable offset : int;
}

let index (p : program) (n : int) : int =
    let mode : int = match n with
        | 1 -> (p.xs.(p.i) / 100) mod 10
        | 2 -> (p.xs.(p.i) / 1000) mod 10
        | 3 -> (p.xs.(p.i) / 10000) mod 10
        | _ -> exit 1 in
    match mode with
        | 0 -> p.xs.(p.i + n)
        | 1 -> p.i + n
        | 2 -> p.xs.(p.i + n) + p.offset
        | _ -> exit 1

let advance (p : program) (j : int) (n : int) : unit =
    if p.i <> j then
        p.i <- p.i + n
    else
        ()

let rec iterate (p : program) (inputs : (int) Queue.t) : unit =
    match p.xs.(p.i) mod 100 with
        | 1 ->
            (let j : int = index p 3 in
             p.xs.(j) <- p.xs.(index p 1) + p.xs.(index p 2);
             advance p j 4;
             iterate p inputs)
        | 2 ->
            (let j : int = index p 3 in
             p.xs.(j) <- p.xs.(index p 1) * p.xs.(index p 2);
             advance p j 4;
             iterate p inputs)
        | 3 ->
            ((try
                 p.xs.(index p 1) <- Queue.take inputs
             with Queue.Empty ->
                 ());
             p.i <- p.i + 2;
             iterate p inputs)
        | 4 ->
            (Printf.fprintf stdout "%d\n" p.xs.(index p 1);
             p.i <- p.i + 2;
             iterate p inputs)
        | 5 ->
            (if p.xs.(index p 1) <> 0 then
                 p.i <- p.xs.(index p 2)
             else
                 p.i <- p.i + 3;
             iterate p inputs)
        | 6 ->
            (if p.xs.(index p 1) = 0 then
                 p.i <- p.xs.(index p 2)
             else
                 p.i <- p.i + 3;
             iterate p inputs)
        | 7 ->
            (let j : int = index p 3 in
             if p.xs.(index p 1) < p.xs.(index p 2) then
                 p.xs.(j) <- 1
             else
                 p.xs.(j) <- 0;
             advance p j 4;
             iterate p inputs)
        | 8 ->
            (let j : int = index p 3 in
             if p.xs.(index p 1) = p.xs.(index p 2) then
                 p.xs.(j) <- 1
             else
                 p.xs.(j) <- 0;
             advance p j 4;
             iterate p inputs)
        | 9 ->
            (let j : int = index p 1 in
             p.offset <- p.offset + p.xs.(j);
             advance p j 2;
             iterate p inputs)
        | 99 -> ()
        | _ -> exit 1

let init (xs : int array) (n : int) : program =
    let p : program = {xs = Array.make 10000 0; i = 0; offset = 0} in
    for i = 0 to (n - 1) do
        p.xs.(i) <- xs.(i)
    done;
    p

let () : unit =
    at_exit (fun () -> flush stdout);
    let xs : int array =
        input_line stdin
        |> String.split_on_char ','
        |> List.map int_of_string
        |> Array.of_list in
    let n : int = Array.length xs in
    iterate (init xs n) ([1] |> List.to_seq |> Queue.of_seq);
    iterate (init xs n) ([2] |> List.to_seq |> Queue.of_seq)
