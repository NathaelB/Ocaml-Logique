let f = Equ(And(Symb "a", Symb "c"),Or(Not(Symb "b"),Imp(Symb "c",And(Bot,Top))));;
let f1 = Equ(And(Symb "a", Symb "b"), Or(Not(Symb "a"), Symb "b"));;

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

let rec ajouteSiPasDedans: string list -> string -> string list = fun li a ->
    match li with
    | [] -> [a]
    | e::l when e!=a -> e::ajouteSiPasDedans l a
    | _::l -> l
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

let rec affichePriBis: prop -> prop -> string = fun fbf symb ->
    let prio = (fun a b -> priority a >= priority b) fbf symb in

    match fbf with
    | Symb _ | Top | Bot -> affiche_symb fbf
    | Not a -> affiche_symb fbf ^ affichePriBis a fbf
    | And (g,d) | Or (g,d) | Imp (g,d) | Equ (g,d) ->
        match (g,d) with
        | _ when prio -> affichePriBis g fbf ^ affiche_symb fbf ^ affichePriBis d fbf
        | _ -> "(" ^ affichePriBis g fbf ^ affiche_symb fbf ^ affichePriBis d fbf ^ ")"
;;

let affichePri: prop -> string = fun fbf ->
    let s = Symb "s" in affichePriBis fbf (Equ (s,s))
;;