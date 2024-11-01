module type Stream = sig
  type 'a t

  exception Empty_stream
  exception Consume_failed
  exception Peek_failed
  exception Single_skip_failed

  val of_list : 'a list -> 'a t
  val is_spent : 'a t -> bool
  val position : 'a t -> int
  val append : 'a t -> 'a list -> unit
  val (<<-) : 'a t -> 'a -> unit
  val dup : 'a t -> 'a t
  val rev : 'a t -> 'a t

  val peek : 'a t -> 'a
  val next : 'a t -> 'a
  val peek_last : 'a t -> 'a
  val npeek : 'a t -> int -> 'a list
  val peek_opt : 'a t -> 'a option
  val npeek_safe : 'a t -> int -> 'a list option
  val peek_ahead : int -> 'a t -> 'a
  val skip_next : 'a t -> 'a t
  val skip : 'a t -> unit

  val remaining : 'a t -> int

  val take_while : ('a -> bool) -> 'a t -> 'a list
  val take_until : ('a -> bool) -> 'a t -> 'a list
  val drop_while : ('a -> bool) -> 'a t -> unit
  val drop_until : ('a -> bool) -> 'a t -> unit
  val skip_while : ('a -> bool) -> 'a t -> 'a t
  val accumulate_into_list : ('a -> bool) -> ('a -> bool) -> ('a -> bool) -> 'a list list
  val skip_single : ('a -> bool) -> 'a t -> unit
end
