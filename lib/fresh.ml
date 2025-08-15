open Syntax

type freshness_env = (ident * uvar) list

let rec is_fresh (env : freshness_env) (a : ident) (t : term) : bool =
  match t with
  | TUnit ->
      true
  | TAbs (b, t') ->
      if a = b then true else is_fresh env a t'
  | TApp (t1, t2)
  | TPair (t1, t2) ->
      is_fresh env a t1 && is_fresh env a t2
  | TAtom b ->
      a <> b
  | TUvar (pi, x) ->
      List.mem (Permutation.apply_ident (List.rev pi) a, x) env
