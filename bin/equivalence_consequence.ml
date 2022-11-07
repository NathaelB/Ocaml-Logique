let rec memeModeles: prop -> prop -> interpretationList ->  bool = fun f1 f2 li ->
  match (f1, f2, li) with
  | (f1,f2, []) -> true
  | (f1,f2, e::l) when (valV f1 e)=(valV f2 e) -> memeModeles f1 f2 l
  | (_,_,_) -> false
;;

let equivalent1: prop -> prop -> bool = fun f1 f2 ->
  memeModeles f1 f2 (ensInt (sp f1))
;;

let equivalent2: prop -> prop -> bool = fun f1 f2 ->
  valide (Equ (f1,f2))
;;

let rec tousSp: prop list -> string list = function
	| [] -> []
	| e::l -> union (sp e) (tousSp l)
;;

let consequence2: prop -> prop -> bool = fun f1 f2 ->
	let ensInt = ensInt (tousSp [f1;f2]) in

	let rec fn: prop -> prop -> interpretationList -> bool = fun a b li ->
		match li with
		| [] -> true
		| e::l ->
			if (modele a e) && not(modele b e) then false
			else fn a b l in
	fn f1 f2 ensInt
;;

let rec modeleCommun: prop list -> interpretation -> bool = fun li i ->
  match li with
  | [] -> true
  | e::l ->
      if modele e i then modeleCommun l i
      else false
;;

let rec consequence: prop list -> prop -> bool = fun hs c ->
  let ensInt = (ensInt (tousSp hs)) in

  let rec fn: prop list -> prop -> interpretationList -> bool = fun a b li ->
    match li with
    | [] -> true
    | e::l ->
        if (modeleCommun a e) && not(modele b e) then false
        else fn a b l in
  fn hs c ensInt
;;


let rec conjonction: prop list -> prop = function
  | [] -> Bot
  | e::l ->
      if l=[] then e
      else And(e, conjonction l)
;;

let consequenceV: prop list -> prop -> bool = fun hs c ->
  valide (Imp(conjonction hs, c))
;;

let consequenceI: prop list -> prop -> bool = fun hs c ->
  insatisfiable (And(conjonction hs, Not(c)))
;;