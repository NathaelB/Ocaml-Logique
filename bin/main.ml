#use "types.ml";;
#use "tools.ml";;

(*
@params: prop
@return int
*)
let priority: prop -> int = function
  | Equ(g,d) -> 0
  | Imp(g,d) -> 1
  | Or (g,d) -> 2
  | And(g,d) -> 3
  | Not(a) -> 4
  | _ -> 5
;;

(*
@params: prop
@return: string
-> affiche_symb (And(Symb "a", Symb "b"))
*)
let affiche_symb: prop -> string = function
  | Symb a ->  a
  | Top ->  "⊤"
  | Bot ->  "⊥"
  | Not _ ->  "¬"
  | And (_,_) ->"∧"
  | Or (_,_) -> "∨"
  | Imp (_,_) ->"⇒"
  | Equ (_,_) -> "⇔"
;;

#use "syntaxe.ml";;
#use "semantique.ml";;
#use "satisfiabilite.ml";;
#use "equivalence_consequence.ml";;
