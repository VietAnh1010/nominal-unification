open Syntax
open Fresh

let rec is_equal (env : freshness_env) (t1 : term) (t2 : term) : bool =
  match t1, t2 with
  | TUnit, TUnit ->
      true
  | TAbs (a1, t1'), TAbs (a2, t2') when a1 = a2 ->
      is_equal env t1' t2'
  | TAbs (a1, t1'), TAbs (a2, t2') ->
      is_fresh env a1 t2' && is_equal env t1' (Permutation.apply_term [(a1, a2)] t2')
  | TApp (t11, t12), TApp (t21, t22)
  | TPair (t11, t12), TPair (t21, t22) ->
      is_equal env t11 t21 && is_equal env t12 t22
  | TAtom a1, TAtom a2 ->
      a1 = a2
  | TUvar _, TUvar _ ->
      assert false
  | _, _ ->
      false
