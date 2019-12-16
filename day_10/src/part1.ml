(* Day 10: Monitoring Station (Part 1) *)

let read_file (filename : string) : string list =
    let lines : string list ref = ref [] in
    let chan : in_channel = open_in filename in
    (try
        while true; do
            lines := input_line chan :: !lines
        done
    with End_of_file ->
        close_in chan);
    List.rev !lines

(*  NOTE: x == j == column
          y == i == row *)

type position = {
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
    List.init (String.length s) (String.get s)
    |> loop [] 0 0
    |> List.rev

type relation = {
    slope : float;
    right : bool;
}

(*  NOTE: For a 10x10 grid:
        (0, 0) <- top left
        (9, 9) <- bottom right
    Never forget this! *)

let relate (a : position) (b : position) : relation =
    let x : int = a.x - b.x in
    let y : int = a.y - b.y in
    {
        slope = (float_of_int x) /. (float_of_int y);
        right = y < 0;
    }

let iterate (ps : position array) : (position * int) =
    let n : int = (Array.length ps) - 1 in
    let calculate (i : int) : int =
        let table : (relation, bool) Hashtbl.t = Hashtbl.create n in
        for j = 0 to n do
            if i <> j then
                let r : relation = relate ps.(i) ps.(j) in
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

let () : unit =
    let lines : string list = read_file Sys.argv.(1) in
    List.iter (Printf.fprintf stdout "%s\n") lines;
    let width : int = List.hd lines |> String.length in
    let ps : position array =
        String.concat "" lines
        |> transform width
        |> Array.of_list in
    iterate ps
    |> (fun (p, v) ->
        Printf.fprintf stdout "{x: %d, y: %d}\n%d\n%!" p.x p.y v)
