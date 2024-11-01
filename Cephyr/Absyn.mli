module type Absyn = sig
  type t
  type top_level
  type typ

  exception No_entrypoint

  val is_definition : top_level -> bool
  val get_entrypoint : t -> entrypoint
end
