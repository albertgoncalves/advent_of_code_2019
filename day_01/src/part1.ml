(* Day 1: The Tyranny of the Rocket Equation (Part 1) *)

let read_file (filename : string) : string list =
    let lines : string list ref = ref [] in
    let chan : in_channel = open_in filename in
    begin
        try
            while true; do
                lines := input_line chan :: !lines
            done
        with End_of_file ->
            close_in chan
    end;
    List.rev !lines

let () : unit =
    try
        read_file Sys.argv.(1)
        |> List.map int_of_string
        |> List.map (fun x -> (x / 3) - 2)
        |> List.fold_left (+) 0
        |> Printf.fprintf stdout "%d\n%!"
    with _ ->
        Printf.fprintf stdout "%s <filename: string>%!" Sys.argv.(0);
        exit 1
