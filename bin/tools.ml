let rec insert: 'a -> 'a list -> 'a list = fun a li ->
    match li with
    | [] -> [a]
    | e::l -> a::e::l
;;
