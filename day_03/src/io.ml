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

let print_move : Terrain.move option -> unit =
    let f : string -> int -> unit = Printf.fprintf stdout "%s\t%d\n%!" in
    function
        | Some (Terrain.Up x) -> f "Up" x
        | Some (Terrain.Down x) -> f "Down" x
        | Some (Terrain.Left x) -> f "Left" x
        | Some (Terrain.Right x) -> f "Right" x
        | None -> ()

let print_bounds (x : Terrain.bounds) : unit =
    Printf.fprintf
        stdout
        "Top\t%d\nBottom\t%d\nLeft\t%d\nRight\t%d\n%!"
        x.Terrain.top
        x.Terrain.bottom
        x.Terrain.left
        x.Terrain.right

let print_grid (g : Terrain.grid) : unit =
    let n : int = Array.length g.Terrain.buffer in
    let rec loop (i : int) : unit =
        if (i < n) then
            begin
                let w : char list =
                    Array.sub g.Terrain.buffer i (min g.Terrain.width (n - i))
                    |> Array.to_list in
                let buf : Buffer.t = Buffer.create g.Terrain.width in
                List.iter (Buffer.add_char buf) w;
                Buffer.contents buf |> Printf.fprintf stdout "%s\n";
                loop (i + g.Terrain.width)
            end
        else
            flush stdout in
    loop 0
