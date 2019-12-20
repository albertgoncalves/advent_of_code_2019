(* Day 10: Monitoring Station (Part 1) | Answer: 299
                              (Part 2) | Answer: 1419 *)

let read_file (filename : string) : string list =
    let lines : string list ref = ref [] in
    let chan : in_channel = open_in filename in (
        try
            while true; do
                lines := input_line chan :: !lines
            done
        with End_of_file ->
            close_in chan
    );
    List.rev !lines

type position = {
    (* NOTE: j|column -> x
             i|row    -> y *)
    x : int;
    y : int;
}

let transform (width : int) (s : string) : position list =
    let n : int = width - 1 in
    let rec loop (ps : position list) (j : int) (i : int)
        : char list -> position list = function
        | [] -> ps
        | c :: cs ->
            let ps : position list =
                if c = '#' then
                    {x = j; y = i} :: ps
                else
                    ps in
            let (j, i) : (int * int) =
                if j < n then
                    (j + 1, i)
                else
                    (0, i + 1) in
            loop ps j i cs in
    List.init (String.length s) (String.get s) |> loop [] 0 0 |> List.rev

type simple_relation = {
    slope : float;
    right : bool;
}

let simple_relate (a : position) (b : position) : simple_relation =
    (* NOTE: (0, 0) <- top left
             (9, 9) <- bottom right *)
    let x : float = b.x - a.x |> float_of_int in
    let y : float = b.y - a.y |> float_of_int in
    {
        slope = y /. x;
        right = a.x < b.x;
    }

let iterate (ps : position array) : (position * int) =
    let n : int = (Array.length ps) - 1 in
    let calculate (i : int) : int =
        let table : (simple_relation, bool) Hashtbl.t = Hashtbl.create n in
        let p : position = ps.(i) in
        for j = 0 to n do
            if i <> j then
                let r : simple_relation = simple_relate p ps.(j) in
                match Hashtbl.find_opt table r with
                    | None -> Hashtbl.add table r true
                    | _ -> ()
            else
                ()
        done;
        Hashtbl.length table in
    let rec loop (i : int) (j : int) (value : int) : (position * int) =
        if i <= n then
            let candidate : int = calculate i in
            if value < candidate then
                loop (i + 1) i candidate
            else
                loop (i + 1) j value
        else
            (ps.(j), value) in
    loop 0 0 0

type relation = {
    x : int;
    y : int;
    radial : float;
    distance : float;
}

let relate (a : position) (b : position) : relation =
    let x : float = b.x - a.x |> float_of_int in
    let y : float = b.y - a.y |> float_of_int in
    {
        x = b.x;
        y = b.y;
        radial = atan2 x y;
        distance = (x *. x) +. (y *. y) |> sqrt;
    }

let order (a : relation) (b : relation) : int =
    if a.radial = b.radial then
        compare a.distance b.distance
    else
        compare b.radial a.radial

let collect (rs : relation list) : relation list array =
    let rec loop (gs : relation list list) (g : relation list)
        (r : float) : relation list -> relation list list = function
        | a :: rs ->
            if a.radial <> r then
                match g with
                    | [] -> loop gs [a] a.radial rs
                    | g -> loop (g :: gs) [a] a.radial rs
            else
                loop gs (a :: g) r rs
        | [] -> gs in
    loop [] [] infinity rs |> List.rev |> Array.of_list

let target (t : int) (gs : relation list array) : relation option =
    let n : int = Array.length gs in
    let rec loop (r : relation option) (i : int) (k : int) =
        if k = t then
            r
        else
            let j =
                if i < n then
                    i + 1
                else
                    0 in
            match gs.(i) with
                | r :: rs -> (
                        gs.(i) <- rs;
                        loop (Some r) j (k + 1)
                    )
                | [] -> loop r j (k + 1) in
    loop None 0 0

let () : unit =
    at_exit (fun () : unit -> flush stdout);
    let lines : string list = read_file Sys.argv.(1) in
    List.iter (Printf.fprintf stdout "%s\n") lines;
    let ps : position array =
        String.concat "" lines
        |> transform (List.hd lines |> String.length)
        |> Array.of_list in
    let (p, v) : (position * int) = iterate ps in
    Printf.fprintf stdout "{x: %d, y: %d}\n%d\n" p.x p.y v;
    let rs : relation array =
        Array.to_seq ps
        |> Seq.filter (fun (x : position) : bool -> x <> p)
        |> Seq.map (relate p)
        |> Array.of_seq in
    Array.sort order rs;
    Array.to_list rs
    |> collect
    |> target 200
    |> Option.iter (
        fun (r : relation) : unit ->
            Printf.fprintf stdout "%d" ((r.x * 100) + r.y)
    )
