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

let ident (ident_name : string) : ident = {ident_name}

let uvar (uvar_name : string) : uvar = {uvar_name}

let t_unit : term = TUnit

let t_abs (a : ident) (t : term) : term = TAbs (a, t)

let t_app (t1 : term) (t2 : term) : term = TApp (t1, t2)

let t_pair (t1 : term) (t2 : term) : term = TApp (t1, t2)

let t_atom (a : ident) = TAtom a

let t_uvar ?(pi : permutation = []) (x : uvar) = TUvar (pi, x)
