open Nominal_unification
open Syntax

let run_example ?(i : int = 0) (lhs : term) (rhs : term) : unit =
  Format.printf "\n\n[EXAMPLE %d]: %s =? %s\n" i (show_term lhs) (show_term rhs);
  match Unify.unify lhs rhs with
  | Error e ->
      Format.printf "\n[ERROR]: %s\n\n" e
  | Ok (sigma, env) ->
      let sigma = Substitution.show_substitution sigma in
      let env = Fresh.show_freshness_env env in
      Format.printf "\n[OK]: sigma = %s; fresh = %s\n\n" sigma env

let example_0 =
  let lhs = t_app (t_abs (ident "a") (t_uvar (uvar "x"))) (t_uvar (uvar "y")) in
  let rhs = t_app (t_abs (ident "b") (t_atom (ident "b"))) (t_pair t_unit (t_atom (ident "c"))) in
  lhs, rhs

let example_1 =
  let lhs = t_abs (ident "a") (t_abs (ident "b") (t_pair (t_uvar (uvar "x")) (t_uvar (uvar "y")))) in
  let rhs = t_abs (ident "c") (t_abs (ident "d") (t_pair (t_atom (ident "c")) (t_atom (ident "d")))) in
  lhs, rhs

let example_2 =
  let lhs = t_abs (ident "a") (t_app (t_atom (ident "a")) (t_atom (ident "b"))) in
  let rhs = t_abs (ident "b") (t_app (t_atom (ident "b")) (t_atom (ident "a"))) in
  lhs, rhs

let examples = [example_0; example_1; example_2]

let () =
  List.iteri (fun i (lhs, rhs) -> run_example ~i lhs rhs) examples
