let filter_option (xs : 'a option list) : 'a list =
    let rec loop (ys : 'a list) : 'a option list -> 'a list = function
        | (Some x) :: xs -> loop (x :: ys) xs
        | None :: xs -> loop ys xs
        | [] -> ys in
    loop [] xs |> List.rev

let () : unit =
    let mss : Terrain.move list list =
        Io.read_file Sys.argv.(1)
        |> List.map (String.split_on_char ',')
        |> List.map (List.map Terrain.parse)
        |> List.map filter_option in
    let b : Terrain.bounds = mss |> Terrain.survey in
    Terrain.print_bounds b;
    let (g, start) : (Terrain.grid * Terrain.position) = b |> Terrain.init in
    match mss with
        | [ms1; ms2] ->
            begin
                let _ : int option = Terrain.iterate '1' g start ms1 in
                match Terrain.iterate '2' g start ms2 with
                    | None -> ()
                    | Some d ->
                        Printf.fprintf stdout "%d\n%!" d
            end
        | _ -> ()
