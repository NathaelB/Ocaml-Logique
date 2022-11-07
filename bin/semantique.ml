let rec intSymb: string -> interpretation -> valVerite = fun s i ->
    match i with
    | [] -> Zero
    | (x,y)::l ->
      if x=s then y
      else intSymb s l
;;

let intNeg: valVerite -> valVerite = function
    | Un -> Zero
    | _ -> Un
;;

let intAnd: valVerite -> valVerite -> valVerite = fun a b ->
  match (a,b) with
  | Un,Un -> Un
  | _, _ -> Zero
;;

let intOr: valVerite -> valVerite -> valVerite = fun a b ->
  match a,b with
  | Zero, Zero -> Zero
  | _,_ -> Un
;;

let intImp: valVerite -> valVerite -> valVerite = fun a b ->
  match a,b with
  | Un, Zero -> Zero
  | _ -> Un
;;

let intEqu: valVerite -> valVerite -> valVerite = fun a b ->
  match a,b with
  | Zero, Zero | Un,Un -> Un
  | _,_ -> Zero
;;

let intTop = Un;;
let intBot = Zero;;

let intTop = Un;;
let intBot = Zero;;

let rec valV: prop -> interpretation -> valVerite = fun f i ->
    match f with
    | Not a -> intNeg (valV a i)
    | And (g,d) -> intAnd (valV g i) (valV d i)
    | Or (g,d) -> intOr (valV g i) (valV d i)
    | Imp (g,d) -> intImp (valV g i) (valV d i)
    | Equ (g,d) -> intEqu (valV g i) (valV d i)
    | Bot -> intBot
    | Top -> intTop
    | Symb a -> intSymb a i
;;

let modele: prop -> interpretation -> bool = fun f i ->
    valV f i = Un
;;





