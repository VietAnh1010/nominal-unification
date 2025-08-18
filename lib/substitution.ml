open Syntax

type substitution = (uvar * term) list
[@@deriving show]

let apply_uvar (sigma : substitution) (pi : permutation) (x : uvar) : term option =
  Option.map (Permutation.apply_term pi) (List.assoc_opt x sigma)

let rec apply_term (sigma : substitution) (t : term) : term =
  match t with
  | TUnit
  | TAtom _ -> t
  | TAbs (a, t') -> TAbs (a, apply_term sigma t')
  | TApp (t1, t2) -> TApp (apply_term sigma t1, apply_term sigma t2)
  | TPair (t1, t2) -> TPair (apply_term sigma t1, apply_term sigma t2)
  | TUvar (pi, x) -> Option.value ~default:t (apply_uvar sigma pi x)
