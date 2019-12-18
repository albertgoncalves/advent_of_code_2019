(* Day 1: The Tyranny of the Rocket Equation (Part 1) *)

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

let () : unit =
    at_exit (fun () -> flush stdout);
    try
        read_file Sys.argv.(1)
        |> List.to_seq
        |> Seq.map int_of_string
        |> Seq.map (fun x -> (x / 3) - 2)
        |> Seq.fold_left (+) 0
        |> Printf.fprintf stdout "%d\n"
    with _ ->
        Printf.fprintf stdout "%s <filename: string>" Sys.argv.(0);
        exit 1
