let read_file (filename : string) : string list =
    let lines : string list ref = ref [] in
    let chan : in_channel = open_in filename in
    try
        while true; do
            lines := input_line chan :: !lines
        done;
        !lines
    with End_of_file ->
        close_in chan;
        List.rev !lines

let () : unit =
    try
        read_file Sys.argv.(1)
        |> List.map int_of_string
        |> List.map (fun x -> (x / 3) - 2)
        |> List.fold_left (+) 0
        |> Printf.fprintf stdout "%d\n%!"
    with _ ->
        Printf.fprintf stdout "%s <filename: string>%!" Sys.argv.(0)
