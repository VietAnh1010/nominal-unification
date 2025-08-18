type ident = {ident_name : string}
[@@deriving show]

type uvar = {uvar_name : string}
[@@deriving show]

type permutation = (ident * ident) list
[@@deriving show]

type term =
  | TUnit
  | TAbs of ident * term
  | TApp of term * term
  | TPair of term * term
  | TAtom of ident
  | TUvar of permutation * uvar
[@@deriving show]
