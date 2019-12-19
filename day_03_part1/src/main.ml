(* Day 3: Crossed Wires (Part 1) | Answer: 375 *)

let newline () : unit = Printf.fprintf stdout "\n"

let () : unit =
    at_exit (fun () -> flush stdout);
    let mss : Terrain.move list list =
        Io.read_file Sys.argv.(1)
        |> List.to_seq
        |> Seq.map (String.split_on_char ',')
        |> Seq.map (List.filter_map Terrain.parse)
        |> List.of_seq in
    let b : Terrain.bounds = mss |> Terrain.survey in
    let void : float = 0.0 in
    let (g, start) : (Terrain.grid * Terrain.position) =
        b |> Terrain.init (-1.0) void in
    let n : int = g.Terrain.buffer |> Array.length in
    let (ms1, ms2) : (Terrain.move list * Terrain.move list) = match mss with
        | [ms1; ms2] -> (ms1, ms2)
        | _ -> exit 1 in
    ignore (Terrain.iterate 1.0 void g start ms1);
    let x : int = match Terrain.iterate 2.0 void g start ms2 with
        | Some x -> x
        | None -> exit 1 in
    Printf.fprintf stdout "%d\n" x;
    newline ();
    Terrain.print_bounds b;
    n |> Printf.fprintf stdout "Size\t%d\n";
    if n < 128 then (
        List.iter (
            fun ms ->
                newline ();
                Terrain.print_moves ms
        )
            mss;
        newline ();
        Terrain.print_grid void g
    ) else
        ()
