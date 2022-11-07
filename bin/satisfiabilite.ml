#use "syntaxe.ml";;
let rec consTous: 'a -> 'a list list -> 'a list list = fun a li ->
    match li with
    | [] -> []
    | e::l -> insert a e ::consTous a l
;;

let rec ensInt: string list -> interpretationList = function
    | [] -> [[]]
    | e::l -> let ensIl = (ensInt l) in (consTous (e, Zero) ensIl) @ (consTous (e, Un) ensIl)
;;

let rec existeModele: prop -> interpretationList -> bool = fun fbf i ->
  match i with
  | [] -> false
  | e::l ->
      if modele fbf e=true then true
      else existeModele fbf l
;;

let rec tousModele: prop -> interpretationList -> bool = fun fbf listInt ->
  match listInt with
  | [] -> true
  | e::l ->
    if (valV fbf e) = Zero then false
		else tousModele fbf l
;;

let satisfiable: prop -> bool = fun fbf ->
  tousModele fbf (ensInt(sp fbf))
;;


let valide: prop -> bool = fun f ->
  tousModele f (ensInt( sp f))
;;

let insatisfiable: prop -> bool = fun fbf ->
  not (satisfiable fbf )
;;