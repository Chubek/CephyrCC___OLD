module type HLInter = sig
  type t
  type lvalue
  type lbase
  type loffs
  type exp
  type instr
  type stmt
  type typ

  val empty : t
  val (<<-) : t -> stmt -> t

  let insert_lvsym : t -> string -> lvalue -> unit
  let insert_typsym : t -> string -> typ -> unit

  let get_lvsym : t -> string -> lvalue
  let get_typsym : t -> string -> typ
end
