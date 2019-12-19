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
