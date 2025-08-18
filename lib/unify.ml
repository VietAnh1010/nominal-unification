open Syntax

type unification_state = {
  us_equal_problems : (term * term) list;
  us_fresh_problems : (ident * term) list;
  us_substitution : Substitution.substitution;
  us_freshness_env : Fresh.freshness_env;
}
[@@deriving show]

let unify_fresh_step (st : unification_state) (a : ident) (t : term) : (unification_state, string) result =
  match t with
  | TUnit ->
      Result.ok st
  | TAbs (a', _) when a = a' ->
      Result.ok st
  | TAbs (_, t') ->
      let st = {st with us_fresh_problems = (a, t') :: st.us_fresh_problems} in
      Result.ok st
  | TApp (t1, t2)
  | TPair (t1, t2) ->
      let st = {st with us_fresh_problems = (a, t1) :: (a, t2) :: st.us_fresh_problems} in
      Result.ok st
  | TAtom a' when a = a' ->
      Result.error (Format.sprintf "%s is not free" (show_ident a))
  | TAtom _ ->
      Result.ok st
  | TUvar (pi, x) ->
      let env = [(Permutation.apply_ident (List.rev pi) a, x)] in
      let st = {st with us_freshness_env = env @ st.us_freshness_env} in
      Result.ok st

let rec unify_fresh_loop (st : unification_state) : (unification_state, string) result =
  let open Utils.Results in
  match st.us_fresh_problems with
  | [] ->
      Result.ok st
  | (a, t) :: ps ->
      let st = {st with us_fresh_problems = ps} in
      let* st = unify_fresh_step st a t in
      unify_fresh_loop st

let unify_equal_step (st : unification_state) (t1 : term) (t2 : term) : (unification_state, string) result =
  match t1, t2 with
  | TUnit, TUnit ->
      Result.ok st
  | TAbs (a1, t1'), TAbs (a2, t2') when a1 = a2 ->
      let st = {st with us_equal_problems = (t1', t2') :: st.us_equal_problems} in
      Result.ok st
  | TAbs (a1, t1'), TAbs (a2, t2') ->
      let t2'' = Permutation.apply_term [(a1, a2)] t2' in
      let st = {st with
        us_equal_problems = (t1', t2'') :: st.us_equal_problems;
        us_fresh_problems = (a1, t2') :: st.us_fresh_problems} in
      Result.ok st
  | TApp (t11, t12), TApp (t21, t22)
  | TPair (t11, t12), TPair (t21, t22) ->
      let st = {st with us_equal_problems = (t11, t21) :: (t12, t22) :: st.us_equal_problems} in
      Result.ok st
  | TAtom a1, TAtom a2 when a1 = a2 ->
      Result.ok st
  | TAtom a1, TAtom a2 ->
      Result.error (Format.sprintf "cannot unify different atoms: %s and %s" (show_ident a1) (show_ident a2))
  | TUvar (pi, x), t
  | t, TUvar (pi, x) ->
      let sigma = [x, Permutation.apply_term (List.rev pi) t] in
      let st = {st with
        us_equal_problems = List.map (fun (t1', t2') -> (Substitution.apply_term sigma t1', Substitution.apply_term sigma t2')) st.us_equal_problems;
        us_fresh_problems = List.map (fun (a, t') -> (a, Substitution.apply_term sigma t')) st.us_fresh_problems;
        us_substitution = sigma @ st.us_substitution} in
      Result.ok st
  | _, _ ->
      Result.error (Format.sprintf "cannot unify different constructors: %s and %s" (show_term t1) (show_term t2))

let rec unify_equal_loop (st : unification_state) : (unification_state, string) result =
  let open Utils.Results in
  match st.us_equal_problems with
  | [] ->
      Result.ok st
  | (t1, t2) :: ps ->
      let st = {st with us_equal_problems = ps} in
      let* st = unify_equal_step st t1 t2 in
      unify_equal_loop st

let unify (t1 : term) (t2 : term) : (Substitution.substitution * Fresh.freshness_env, string) result =
  let open Utils.Results in
  let st = {
    us_equal_problems = [(t1, t2)];
    us_fresh_problems = [];
    us_substitution = [];
    us_freshness_env = []} in
  let* st = unify_equal_loop st in
  let* st = unify_fresh_loop st in
  Result.ok (st.us_substitution, st.us_freshness_env)
