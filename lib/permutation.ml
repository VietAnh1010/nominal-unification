open Syntax

let rec apply_ident (pi : permutation) (a : ident) : ident =
  match pi with
  | [] -> a
  | (a1, a2) :: pi' ->
      let a' = apply_ident pi' a in
      if a' = a1 then a2 else
      if a' = a2 then a1 else
      a'

let rec apply_term (pi : permutation) (t : term) : term =
  match t with
  | TUnit -> TUnit
  | TAbs (a, t') -> TAbs (apply_ident pi a, apply_term pi t')
  | TApp (t1, t2) -> TApp (apply_term pi t1, apply_term pi t2)
  | TPair (t1, t2) -> TPair (apply_term pi t1, apply_term pi t2)
  | TAtom a -> TAtom (apply_ident pi a)
  | TUvar (pi', x) -> TUvar (pi @ pi', x)
