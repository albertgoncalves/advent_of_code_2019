type move =
    | Up of int
    | Down of int
    | Left of int
    | Right of int

type bounds = {
    mutable top : int;
    mutable bottom : int;
    mutable left : int;
    mutable right : int;
}

type grid = {
    buffer : char array;
    width : int;
    height : int;
    start : int;
}

let parse (s : string) : move option =
    let key : string = String.sub s 0 1 in
    let value : int =
        String.sub s 1 ((String.length s) - 1) |> int_of_string in
    match key with
        | "U" -> Some (Up value)
        | "D" -> Some (Down value)
        | "L" -> Some (Left value)
        | "R" -> Some (Right value)
        | _ -> None

let survey (mss : move list list) : bounds =
    let b : bounds = {
        top = 0;
        bottom = 0;
        left = 0;
        right = 0;
    } in
    let rec loop (x : int) (y : int) : move list -> unit = function
        | Up m :: ms ->
            let y : int = y + m in
            let _ : unit = b.top <- max b.top y in
            loop x y ms
        | Down m :: ms ->
            let y : int = y - m in
            let _ : unit = b.bottom <- min b.bottom y in
            loop x y ms
        | Left m :: ms ->
            let x : int = x - m in
            let _ : unit = b.left <- min b.left x in
            loop x y ms
        | Right m :: ms ->
            let x : int = x + m in
            let _ : unit = b.right <- max b.right x in
            loop x y ms
        | [] -> () in
    List.iter (loop 0 0) mss;
    b

let select (width : int) (j : int) (i : int) : int = (width * i) + j

let init (b : bounds) : grid =
    let width : int = (b.right - b.left) + 1 in
    let height : int = (b.top - b.bottom) + 1 in
    let start : int = select width (0 - b.left) (height - 1 + b.bottom) in
    let g : grid = {
        buffer = Array.make (width * height) '.';
        width = width;
        height = height;
        start = start;
    } in
    g.buffer.(start) <- 'O';
    g

let print_moves (ms : move list) : unit =
    let f : string -> int -> unit = Printf.fprintf stdout "%s\t%d\n" in
    let g : move -> unit = function
        | Up x -> f "Up" x
        | Down x -> f "Down" x
        | Left x -> f "Left" x
        | Right x -> f "Right" x in
    List.iter g ms;
    flush stdout

let print_bounds (b : bounds) : unit =
    Printf.fprintf
        stdout
        "Top\t%d\nBottom\t%d\nLeft\t%d\nRight\t%d\n%!"
        b.top
        b.bottom
        b.left
        b.right

let print_grid (g : grid) : unit =
    let n : int = Array.length g.buffer in
    let rec loop (i : int) : unit =
        if (i < n) then
            begin
                let w : char array =
                    Array.sub
                        g.buffer
                        i
                        (min g.width (n - i)) in
                let buf : Buffer.t = Buffer.create g.width in
                Array.iter (Buffer.add_char buf) w;
                Buffer.contents buf |> Printf.fprintf stdout "%s\n";
                loop (i + g.width)
            end
        else
            flush stdout in
    loop 0
