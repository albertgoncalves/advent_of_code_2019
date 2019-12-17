(* Day 3: Crossed Wires (Part 1) *)

let rev_filter_map (f : 'a -> 'b option) : 'a list -> 'b list =
    let rec loop (ys : 'b list) : 'a list -> 'b list = function
        | [] -> ys
        | x :: xs ->
            match f x with
                | Some y -> loop (y :: ys) xs
                | None -> loop ys xs in
    loop []

let () : unit =
    let mss : Terrain.move list list =
        Io.read_file Sys.argv.(1)
        |> List.to_seq
        |> Seq.map (String.split_on_char ',')
        |> Seq.map (rev_filter_map Terrain.parse)
        |> Seq.map List.rev
        |> List.of_seq in
    let b : Terrain.bounds = mss |> Terrain.survey in
    Terrain.print_bounds b;
    let (g, start) : (Terrain.grid * Terrain.position) = b |> Terrain.init in
    g.Terrain.buffer |> Array.length |> Printf.fprintf stdout "Size\t%d\n";
    (match mss with
        | [ms1; ms2] ->
            (let _ : int option = Terrain.iterate '1' g start ms1 in
             match Terrain.iterate '2' g start ms2 with
                 | None -> ()
                 | Some d -> Printf.fprintf stdout "\nResult\t%d\n" d)
        | _ -> ());
    flush stdout
