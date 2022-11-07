let f1 = Equ(And(Symb "a", Symb "b"), Or(Not(Symb "a"), Symb "b"));;
let f2 = Or(Not(And(Symb "a", Not(Symb "b"))), Not(Imp(Symb "a", Symb "b")));;
let f3 = And(Not(Imp(Symb "a", Or(Symb "a", Symb "b"))), Not(Not(And(Symb "a", Or(Symb "b", Not(Symb "c"))))));;
let f4 = And(
	Or(Not(Symb "a"), Or(Symb "b", Symb "d")),
	And(
		Or(Not(Symb "d"), Symb "c"),
		And(
			Or(Symb "c", Symb "a"),
			And(
				Or(Not(Symb "c"), Symb "b"),
				And(
					Or(Not(Symb "c"), Not(Symb "b")),
					Or(Not(Symb "b"), Symb "d")
				)
			)
		)
	)
);;

let rec nbc: prop -> int = function
    | Symb a -> 0
    | Top | Bot -> 0
    | Not a -> 1 + nbc a
    | And (g,d) | Or (g,d) | Imp (g,d) | Equ (g,d)
        -> 1 + nbc g + nbc d
;;

let rec prof: prop -> int = function
    | Symb a -> 0
    | Top | Bot -> 0
    | Not a -> 1 + prof a
    | And (g,d) | Or (g,d) | Imp (g,d) | Equ (g,d)
        -> 1 + (max (prof g) (prof d))
;;

let rec ajouteSiPasDedans: string list -> string -> string list = fun liste x ->
  match liste with
  | [] -> [x]
  | e::l ->
      if e=x then e::l
      else e::ajouteSiPasDedans l x
;;

let rec union: string list -> string list -> string list = fun l1 l2 ->
  match (l1,l2) with
  | ([], l2) -> l2
  | (l1, []) -> l1
  | (e::l, x) -> (union l (ajouteSiPasDedans x e))
;;

let rec sp: prop -> string list = function
  | Symb a -> [a]
  | Top | Bot -> []
  | Not (a) -> sp a
  | And(g,d)| Or(g,d) | Imp(g,d) | Equ(g,d)
    -> union (sp g) (sp d)
;;

let rec affiche: prop -> string = fun fbf ->
    match fbf with
    | Symb a -> a
    | Top | Bot -> affiche_symb fbf
    | Not a -> affiche_symb fbf ^ "(" ^ affiche a ^ ")"
    | And (g,d) | Or (g,d) | Imp (g,d) | Equ (g,d)
        -> "(" ^ affiche g ^ affiche_symb fbf ^ affiche d ^ ")"
;;

let rec affichePri2: prop -> prop -> string = fun fbf symb ->
  let prio = (fun a b -> priority a >= priority b) fbf symb in

  match fbf with
  | Symb _
  | Top | Bot -> affiche_symb fbf
  | Not a -> affiche_symb fbf ^ affichePri2 a fbf
  | And (g,d) | Or (g,d) | Imp (g,d) | Equ (g,d)
    -> if prio then affichePri2 g fbf ^ affiche_symb fbf ^ affichePri2 d fbf
      else "(" ^ affichePri2 g fbf ^ affiche_symb fbf ^ affichePri2 d fbf ^ ")"
;;

let affichePri: prop -> string = fun fbf ->
    let s = Symb "s" in affichePriBis fbf (Equ (s,s))
;;