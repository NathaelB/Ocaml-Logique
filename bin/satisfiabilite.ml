let rec consTous: 'a -> 'a list list -> 'a list list = fun a li ->
    match li with
    | [] -> []
    | e::l -> insert a e ::consTous a l
;;

let rec ensInt: string list -> interpretationList = function
    | [] -> [[]]
    | e::l -> let ensIl = (ensInt l) in (consTous (e, Zero) ensIl) @ (consTous (e, Un) ensIl)
;;

let rec existe_modele: prop -> interpretationList -> bool = fun f eI ->
    match eI with
    | [] -> false
    | e::l when modele f e = true -> true
    | _::l -> existe_modele f l
;;

let rec tous_modele: prop -> interpretationList -> bool = fun f eI ->
    match eI with
    | [] -> true
    | e::l when (valV f e) = Zero -> false
    | _::l -> tous_modele f l
;;

let satisfiable: prop -> bool = fun f ->
    existe_modele f (ensInt (sp f))
;;

let valide: prop -> bool = fun f ->
    tous_modele f (ensInt (sp f))
;;

let insatisfiable: prop -> bool = fun f ->
    not (satisfiable f)
;;