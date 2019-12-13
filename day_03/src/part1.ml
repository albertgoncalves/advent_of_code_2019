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
    List.iter (fun ms -> Io.print_moves ms; Printf.fprintf stdout "\n%!") mss;
    let (g, _) : (Terrain.grid * int) =
        mss
        |> Terrain.survey
        |> Terrain.init in
    Io.print_grid g
