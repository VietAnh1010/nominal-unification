type ident = {ident_name : string}

type uvar = {uvar_name : string}

type permutation = (ident * ident) list

type term =
  | TUnit
  | TAbs of ident * term
  | TApp of term * term
  | TPair of term * term
  | TAtom of ident
  | TUvar of permutation * uvar
