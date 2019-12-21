(* Day 12: The N-Body Problem (Part 1) | Answer: 8960 *)

type coord = {
    mutable x : int;
    mutable y : int;
    mutable z : int;
}

type body = {
    pos : coord;
    vel : coord;
}

let data : body array = [|
    {
        pos = {
            x = 17;
            y = -12;
            z = 13;
        };
        vel = {
            x = 0;
            y = 0;
            z = 0;
        }
    };
    {
        pos = {
            x = 2;
            y = 1;
            z = 1;
        };
        vel = {
            x = 0;
            y = 0;
            z = 0;
        }
    };
    {
        pos = {
            x = -1;
            y = -17;
            z = 7;
        };
        vel = {
            x = 0;
            y = 0;
            z = 0;
        }
    };
    {
        pos = {
            x = 12;
            y = -14;
            z = 18;
        };
        vel = {
            x = 0;
            y = 0;
            z = 0;
        }
    };
|]

let n : int = Array.length data

let calculate (i : int) (j : int) : unit =
    (
        if data.(i).pos.x < data.(j).pos.x then (
            data.(i).vel.x <- data.(i).vel.x + 1;
            data.(j).vel.x <- data.(j).vel.x - 1;
        ) else if data.(j).pos.x < data.(i).pos.x then (
            data.(i).vel.x <- data.(i).vel.x - 1;
            data.(j).vel.x <- data.(j).vel.x + 1;
        ) else
            ()
    );
    (
        if data.(i).pos.y < data.(j).pos.y then (
            data.(i).vel.y <- data.(i).vel.y + 1;
            data.(j).vel.y <- data.(j).vel.y - 1;
        ) else if data.(j).pos.y < data.(i).pos.y then (
            data.(i).vel.y <- data.(i).vel.y - 1;
            data.(j).vel.y <- data.(j).vel.y + 1;
        ) else
            ()
    );
    (
        if data.(i).pos.z < data.(j).pos.z then (
            data.(i).vel.z <- data.(i).vel.z + 1;
            data.(j).vel.z <- data.(j).vel.z - 1;
        ) else if data.(j).pos.z < data.(i).pos.z then (
            data.(i).vel.z <- data.(i).vel.z - 1;
            data.(j).vel.z <- data.(j).vel.z + 1;
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
        data.(i).pos.x <- data.(i).pos.x + data.(i).vel.x;
        data.(i).pos.y <- data.(i).pos.y + data.(i).vel.y;
        data.(i).pos.z <- data.(i).pos.z + data.(i).vel.z
    done

let result (b : body) : int =
    ((abs b.pos.x) + (abs b.pos.y) + (abs b.pos.z))
    * ((abs b.vel.x) + (abs b.vel.y) + (abs b.vel.z))

let print_body (b : body) : unit =
    Printf.fprintf
        stdout
        "pos=<x=%d, y=%d, z=%d>, vel=<x=%d, y=%d, z=%d>\n"
        b.pos.x
        b.pos.y
        b.pos.z
        b.vel.x
        b.vel.y
        b.vel.z

let newline () : unit = Printf.fprintf stdout "\n"

let () : unit =
    at_exit (fun () -> flush stdout);
    for _ = 0 to 999 do
        iterate ();
        update ();
    done;
    Array.iter print_body data;
    Printf.fprintf
        stdout
        "%d\n"
        (Array.to_seq data |> Seq.map result |> Seq.fold_left (+) 0)
