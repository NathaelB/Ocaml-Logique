type prop =
  | Symb of string
  | Top
  | Bot
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Imp of prop * prop
  | Equ of prop * prop
;;

type valVerite =
    | Zero
    | Un
;;

type interpretation = (string * valVerite) list;;
type interpretationList = interpretation list;;