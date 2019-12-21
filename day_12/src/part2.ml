(* Day 12: The N-Body Problem (Part 2) | Answer: 314917503970904 *)

type dimension = {
    mutable pos : int;
    mutable vel : int
}

type counter = {
    reference : dimension array;
    mutable n : int;
    mutable flag : bool;
}

let n : int = 4

(* NOTE: Due to how `array`s of `record`s work, the `dimension array`s must be
   identical `dimension array` literals to allow `counter.reference` to remain
   unaltered by `iterate (); update ();`. `Array.copy` will *not* do the trick,
   as the underlying `dimension` records are still referenced via shared
   pointers. *)
let x : dimension array = [|
    {pos = 17; vel = 0};
    {pos = 2; vel = 0};
    {pos = -1; vel = 0};
    {pos = 12; vel = 0};
|]

let counter_x = {
    reference = [|
        {pos = 17; vel = 0};
        {pos = 2; vel = 0};
        {pos = -1; vel = 0};
        {pos = 12; vel = 0};
    |];
    n = 1;
    flag = false;
}

let y : dimension array = [|
    {pos = -12; vel = 0};
    {pos = 1; vel = 0};
    {pos = -17; vel = 0};
    {pos = -14; vel = 0};
|]

let counter_y = {
    reference = [|
        {pos = -12; vel = 0};
        {pos = 1; vel = 0};
        {pos = -17; vel = 0};
        {pos = -14; vel = 0};
    |];
    n = 1;
    flag = false;
}

let z : dimension array = [|
    {pos = 13; vel = 0};
    {pos = 1; vel = 0};
    {pos = 7; vel = 0};
    {pos = 18; vel = 0};
|]

let counter_z = {
    reference = [|
        {pos = 13; vel = 0};
        {pos = 1; vel = 0};
        {pos = 7; vel = 0};
        {pos = 18; vel = 0};
    |];
    n = 1;
    flag = false;
}

let calculate (i : int) (j : int) : unit =
    (
        if x.(i).pos < x.(j).pos then (
            x.(i).vel <- x.(i).vel + 1;
            x.(j).vel <- x.(j).vel - 1;
        ) else if x.(j).pos < x.(i).pos then (
            x.(i).vel <- x.(i).vel - 1;
            x.(j).vel <- x.(j).vel + 1;
        ) else
            ()
    );
    (
        if y.(i).pos < y.(j).pos then (
            y.(i).vel <- y.(i).vel + 1;
            y.(j).vel <- y.(j).vel - 1;
        ) else if y.(j).pos < y.(i).pos then (
            y.(i).vel <- y.(i).vel - 1;
            y.(j).vel <- y.(j).vel + 1;
        ) else
            ()
    );
    (
        if z.(i).pos < z.(j).pos then (
            z.(i).vel <- z.(i).vel + 1;
            z.(j).vel <- z.(j).vel - 1;
        ) else if z.(j).pos < z.(i).pos then (
            z.(i).vel <- z.(i).vel - 1;
            z.(j).vel <- z.(j).vel + 1;
        ) else
            ()
    )

let iterate () : unit =
    for i = 0 to (n - 1) do
        for j = (i + 1) to (n - 1) do
            calculate i j
        done
    done

let update () : unit =
    for i = 0 to (n - 1) do
        x.(i).pos <- x.(i).pos + x.(i).vel;
        y.(i).pos <- y.(i).pos + y.(i).vel;
        z.(i).pos <- z.(i).pos + z.(i).vel
    done

let rec gcd (a : int) (b : int) : int =
    match (a mod b) with
        | 0 -> b
        | r -> gcd b r

let lcm (a : int) (b : int) : int = (a * b) / (gcd a b)

let rec tally () : int =
    if counter_x.flag && counter_y.flag && counter_z.flag then (
        Printf.fprintf stdout "%d %d %d\n" counter_x.n counter_y.n counter_z.n;
        lcm counter_x.n (lcm counter_y.n counter_z.n)
    ) else (
        iterate ();
        update ();
        (
            if (not counter_x.flag) && (counter_x.reference <> x) then
                counter_x.n <- counter_x.n + 1
            else if (not counter_x.flag) then
                counter_x.flag <- true
            else
                ()
        );
        (
            if (not counter_y.flag) && (counter_y.reference <> y) then
                counter_y.n <- counter_y.n + 1
            else if (not counter_y.flag) then
                counter_y.flag <- true
            else
                ()
        );
        (
            if (not counter_z.flag) && (counter_z.reference <> z) then
                counter_z.n <- counter_z.n + 1
            else if (not counter_z.flag) then
                counter_z.flag <- true
            else
                ()
        );
        tally ()
    )

let () : unit =
    at_exit (fun () : unit -> flush stdout);
    Printf.fprintf stdout "%d\n" (tally ())
