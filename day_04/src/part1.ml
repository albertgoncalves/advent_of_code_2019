(* Day 4: Secure Container (Part 1) | Answer: 481 *)

let stdin_to_pair ((): unit) : (int * int) =
    let xs : int option list =
        input_line stdin
        |> String.split_on_char '-'
        |> List.map int_of_string_opt in
    match xs with
        | [Some lower; Some upper] -> (lower, upper)
        | _ -> exit 1

let int_to_digits (x : int) : int list =
    x
    |> string_of_int
    |> (
        fun (y : string) : char list ->
            List.init (String.length y) (String.get y)
    )
    |> List.map (fun (y : char) : int -> Char.escaped y |> int_of_string)

let validate (xs : int list) : bool =
    let rec loop (flag : bool) : int list -> bool = function
        | a :: b :: xs ->
            if b < a then (* NOTE: Halt early if decreasing values found. *)
                false
            else if (not flag) && (a = b) then
                loop true (b :: xs)
            else
                loop flag (b :: xs)
        (* NOTE: Terminate returning iterated flag; will return `true` if there
                 is at least one duplicate value. *)
        | _ -> flag in
    loop false xs

let rec iterate (current : int) (stop : int) (n : int) : int =
    if stop < current then
        n
    else if (int_to_digits current |> validate) then
        iterate (current + 1) stop (n + 1)
    else
        iterate (current + 1) stop n

let () : unit =
    assert ((int_to_digits 123456) = [1; 2; 3; 4; 5; 6]);
    assert ((int_to_digits 111111 |> validate) = true);
    assert ((int_to_digits 223450 |> validate) = false);
    assert ((int_to_digits 123789 |> validate) = false);
    let (lower, upper) : (int * int) = stdin_to_pair () in
    iterate lower upper 0 |> Printf.fprintf stdout "%d\n%!"
