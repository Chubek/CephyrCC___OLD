module type Absyn = sig
  type t
  type top_level
  type typ
  type typqual
  type typkind
  type fsig
  type vsig
  type strgcls
  type idx
  type infix_exp
  type prefix_exp
  type postfix_exp
  type const_val
  type binary_op
  type unary_op
  type exp
  type rvalue
  type init
  type scope
  type stmt

  exception No_entrypoint

  val is_definition : top_level -> bool
  val get_entrypoint : t -> entrypoint
end
