(* Day 11: Space Police (Part 1) | Answer: 1771
                        (Part 2) | Answer: HGEHJHUZ *)

let terrain : ((int * int), int) Hashtbl.t = Hashtbl.create 100

type position = {
    mutable x : int;
    mutable y : int;
    mutable orientation : int;
}

let move (p : position) (a : int) (b : int) : unit =
    p.orientation <- (
        match b with
            | 0 ->
                if p.orientation = 0 then
                    270
                else
                    p.orientation - 90
            | 1 ->
                if p.orientation = 270 then
                    0
                else
                    p.orientation + 90
            | _ -> exit 1
    );
    (
        let xy : (int * int) = (p.x, p.y) in
        match Hashtbl.find_opt terrain xy with
            | None -> Hashtbl.add terrain xy a
            | Some _ -> Hashtbl.replace terrain xy a
    );
    match p.orientation with
        | 0 -> p.y <- p.y - 1
        | 90 -> p.x <- p.x + 1
        | 180 -> p.y <- p.y + 1
        | 270 -> p.x <- p.x - 1
        | _ -> exit 1

type program = {
    xs : int array;
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

let rec iterate (p : program) (q : position) (stack : int list ref) : unit =
    match p.xs.(p.i) mod 100 with
        | 1 -> (
                let j : int = index p 3 in
                p.xs.(j) <- p.xs.(index p 1) + p.xs.(index p 2);
                advance p j 4;
                iterate p q stack
            )
        | 2 -> (
                let j : int = index p 3 in
                p.xs.(j) <- p.xs.(index p 1) * p.xs.(index p 2);
                advance p j 4;
                iterate p q stack
            )
        | 3 -> (
                p.xs.(index p 1) <- (
                    match Hashtbl.find_opt terrain (q.x, q.y) with
                        | Some v -> v
                        | None -> 0
                );
                p.i <- p.i + 2;
                iterate p q stack
            )
        | 4 -> (
                stack := p.xs.(index p 1) :: !stack;
                (
                    match !stack with
                        | [b; a] -> (
                                move q a b;
                                stack := []
                            )
                        | _ -> ()
                );
                p.i <- p.i + 2;
                iterate p q stack
            )
        | 5 -> (
                if p.xs.(index p 1) <> 0 then
                    p.i <- p.xs.(index p 2)
                else
                    p.i <- p.i + 3;
                iterate p q stack
            )
        | 6 -> (
                if p.xs.(index p 1) = 0 then
                    p.i <- p.xs.(index p 2)
                else
                    p.i <- p.i + 3;
                iterate p q stack
            )
        | 7 -> (
                let j : int = index p 3 in
                if p.xs.(index p 1) < p.xs.(index p 2) then
                    p.xs.(j) <- 1
                else
                    p.xs.(j) <- 0;
                advance p j 4;
                iterate p q stack
            )
        | 8 -> (
                let j : int = index p 3 in
                if p.xs.(index p 1) = p.xs.(index p 2) then
                    p.xs.(j) <- 1
                else
                    p.xs.(j) <- 0;
                advance p j 4;
                iterate p q stack
            )
        | 9 -> (
                let j : int = index p 1 in
                p.offset <- p.offset + p.xs.(j);
                advance p j 2;
                iterate p q stack
            )
        | 99 -> ()
        | _ -> exit 1

let init (xs : int array) (n : int) : program =
    let p : program = {
        xs = Array.make 10000 0;
        i = 0;
        offset = 0;
    } in
    for i = 0 to (n - 1) do
        p.xs.(i) <- xs.(i)
    done;
    p

let bounds ((x, y) : (int * int)) _
    ((min_x, max_x, min_y, max_y) : (int * int * int * int))
    : (int * int * int * int) =
    (min x min_x, max x max_x, min y min_y, max y max_y)

let rec print_array (m : int) (n : int) (i : int) (xs : int array) : unit =
    if i < m then (
        Array.sub xs i n
        |> Array.to_list
        |> List.map (
            function
                | 0 -> "_"
                | 1 -> "#"
                | 2 -> " "
                | _ -> exit 1
        )
        |> String.concat ""
        |> Printf.fprintf stdout "%s\n";
        print_array m n (i + n) xs
    ) else
        ()

let () : unit =
    at_exit (fun () : unit -> flush stdout);
    let xs : int array =
        input_line stdin
        |> String.split_on_char ','
        |> List.map int_of_string
        |> Array.of_list in
    let n : int = Array.length xs in
    iterate (init xs n) {x = 0; y = 0; orientation = 0} (ref []);
    Printf.fprintf stdout "%d\n" (Hashtbl.length terrain);
    Hashtbl.clear terrain;
    Hashtbl.add terrain (0, 0) 1;
    iterate (init xs n) {x = 0; y = 0; orientation = 0} (ref []);
    let (min_x, max_x, min_y, max_y) : (int * int * int * int) =
        Hashtbl.fold
            bounds
            terrain
            (max_int, min_int, max_int, min_int) in
    let delta_x : int = max_x - min_x in
    let delta_y : int = max_y - min_y in
    let m : int = (delta_x + 1) * (delta_y + 1) in
    let canvas : int array = Array.make m 0 in
    Hashtbl.iter (
        fun ((x, y) : (int * int)) (v : int) : unit ->
            let i : int = ((y - min_y) * delta_x) + (x - min_x) in
            canvas.(i) <- v
    )
        terrain;
    print_array (m - delta_x) delta_x 0 canvas
