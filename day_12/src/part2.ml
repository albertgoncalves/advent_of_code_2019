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

type container = {
    dim : dimension array;
    ctr : counter;
}

let n : int = 4

let init (a : int) (b : int) (c : int) (d : int) : container =
    (* NOTE: Due to how `array`s of `record`s work, the `dimension array`s must
       be identical `dimension array` literals to allow `counter.reference` to
       remain unaltered by `iterate (); update ();`. `Array.copy` will *not* do
       the trick, as the underlying `dimension` records are still referenced
       via shared pointers. *)
    {
        dim = [|
            {pos = a; vel = 0};
            {pos = b; vel = 0};
            {pos = c; vel = 0};
            {pos = d; vel = 0};
        |];
        ctr = {
            reference = [|
                {pos = a; vel = 0};
                {pos = b; vel = 0};
                {pos = c; vel = 0};
                {pos = d; vel = 0};
            |];
            n = 1;
            flag = false;
        };
    }

let x : container = init 17 2 (-1) 12
let y : container = init (-12) 1 (-17) (-14)
let z : container = init 13 1 7 18

let calculate (i : int) (j : int) : unit =
    (
        if x.dim.(i).pos < x.dim.(j).pos then (
            x.dim.(i).vel <- x.dim.(i).vel + 1;
            x.dim.(j).vel <- x.dim.(j).vel - 1;
        ) else if x.dim.(j).pos < x.dim.(i).pos then (
            x.dim.(i).vel <- x.dim.(i).vel - 1;
            x.dim.(j).vel <- x.dim.(j).vel + 1;
        ) else
            ()
    );
    (
        if y.dim.(i).pos < y.dim.(j).pos then (
            y.dim.(i).vel <- y.dim.(i).vel + 1;
            y.dim.(j).vel <- y.dim.(j).vel - 1;
        ) else if y.dim.(j).pos < y.dim.(i).pos then (
            y.dim.(i).vel <- y.dim.(i).vel - 1;
            y.dim.(j).vel <- y.dim.(j).vel + 1;
        ) else
            ()
    );
    (
        if z.dim.(i).pos < z.dim.(j).pos then (
            z.dim.(i).vel <- z.dim.(i).vel + 1;
            z.dim.(j).vel <- z.dim.(j).vel - 1;
        ) else if z.dim.(j).pos < z.dim.(i).pos then (
            z.dim.(i).vel <- z.dim.(i).vel - 1;
            z.dim.(j).vel <- z.dim.(j).vel + 1;
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
        x.dim.(i).pos <- x.dim.(i).pos + x.dim.(i).vel;
        y.dim.(i).pos <- y.dim.(i).pos + y.dim.(i).vel;
        z.dim.(i).pos <- z.dim.(i).pos + z.dim.(i).vel
    done

let rec gcd (a : int) (b : int) : int =
    match (a mod b) with
        | 0 -> b
        | c -> gcd b c

let lcm (a : int) (b : int) : int = (a * b) / (gcd a b)

let rec tally () : int =
    if x.ctr.flag && y.ctr.flag && z.ctr.flag then (
        Printf.fprintf stdout "%d %d %d\n" x.ctr.n y.ctr.n z.ctr.n;
        lcm x.ctr.n (lcm y.ctr.n z.ctr.n)
    ) else (
        iterate ();
        update ();
        (
            if (not x.ctr.flag) && (x.ctr.reference <> x.dim) then
                x.ctr.n <- x.ctr.n + 1
            else if (not x.ctr.flag) then
                x.ctr.flag <- true
            else
                ()
        );
        (
            if (not y.ctr.flag) && (y.ctr.reference <> y.dim) then
                y.ctr.n <- y.ctr.n + 1
            else if (not y.ctr.flag) then
                y.ctr.flag <- true
            else
                ()
        );
        (
            if (not z.ctr.flag) && (z.ctr.reference <> z.dim) then
                z.ctr.n <- z.ctr.n + 1
            else if (not z.ctr.flag) then
                z.ctr.flag <- true
            else
                ()
        );
        tally ()
    )

let () : unit =
    at_exit (fun () : unit -> flush stdout);
    Printf.fprintf stdout "%d\n" (tally ())
