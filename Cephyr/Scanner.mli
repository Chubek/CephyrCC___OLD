module type Scanner = sig
  type t
  type token

  exception Blank_file
  exception End_of_input
  exception Wrong_symbol of char * int
  exception Unexpected_eof

  val (++) : t -> token -> t
  val (>>) : t -> t

  val implode_while : (char -> bool) -> string
  val implode_until : (char -> bool) -> string
  val implode_while' : (char -> bool) -> string
  val implode_until' : (char -> bool) -> string

  val is_whitespace : char -> bool
  val is_letter : char -> bool
  val is_digit : char -> bool
  val is_xdigit : char -> bool
  val is_odigit : char -> bool
  val is_bdigit : char -> bool
  val is_zinteger : char -> bool
  val is_xinteger : char -> bool
  val is_ointeger : char -> bool
  val is_binteger : char -> bool
  val is_size_letter : char -> bool
  val is_real_num : char -> bool
  val is_operator : char -> bool

  val assess_keyword : string -> token

  val int_of_zlexeme : string -> int64
  val int_of_xlexeme : string -> int64
  val int_of_olexeme : string -> int64
  val int_of_blexeme : string -> int64
  val real_of_rlxeme : string -> float

  val is_valid_end : token -> bool

  val scan : t
end
