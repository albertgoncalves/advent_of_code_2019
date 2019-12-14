(*  $ cat input.txt
    372037-905157 *)
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
    |> (fun y -> List.init (String.length y) (String.get y))
    |> List.map (fun y -> Char.escaped y |> int_of_string)

let validate (xs : int list) : bool =
    let rec f (a : int) (b : int) (flag : bool) (condition : bool)
            (xs : int list) : bool =
        if b < a then
            false
        else if condition then
            loop a true xs
        else
            loop a flag xs
    and loop (previous : int) (flag : bool) : int list -> bool = function
        | a :: b :: c :: xs ->
            f a b flag ((a = b) && (a <> previous) && (a <> c)) (b :: c :: xs)
        | a :: b :: xs ->
            f a b flag ((a = b) && (a <> previous)) (b :: xs)
        | _ -> flag in
    loop 0 false xs

let rec iterate (current : int) (stop : int) (n : int) : int =
    if stop < current then
        n
    else if (int_to_digits current |> validate) then
        iterate (current + 1) stop (n + 1)
    else
        iterate (current + 1) stop n

let () : unit =
    assert ((int_to_digits 123456) = [1; 2; 3; 4; 5; 6]);
    assert ((int_to_digits 112233 |> validate) = true);
    assert ((int_to_digits 123444 |> validate) = false);
    assert ((int_to_digits 111122 |> validate) = true);
    let (lower, upper) : (int * int) = stdin_to_pair() in
    Printf.fprintf stdout "%d\n%d\n%d\n%!" lower upper (iterate lower upper 0)
