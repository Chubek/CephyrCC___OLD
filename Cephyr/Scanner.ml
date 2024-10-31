module Scanner = struct
  type t =
    { token_stream : token Stream.t
    ; char_stream  : char Stream.t
    }

  and token =
    | Operator of string
    | Identifier of string
    | Keyword of string
    | StringLiteral of string
    | CharacterLiteral of string
    | EncodingPrefix of string
    | IntegerLiteral of string
    | HexLiteral of string
    | BinLiteral of string
    | OctLiteral of string
    | BooleanLiteral of string
    | LeftDelimiter of string
    | RightDelimiter of string
    | SeparatorPunctuation of string

  exception Blank_file
  exception End_of_input
  exception Wrong_symbol of char * int
  exception Unexpected_eof of int

  let (++) scn tok =
    Stream.append scn.token_stream [tok]

  let (>>) scn =
    Stream.skip scn.char_stream;
    scn

  let implode_while pred scn =
    let lst = Stream.take_while pred scn.char_stream in
    String.of_seq (List.to_seq lst)

  let implode_until pred scn =
    let lst = Stream.take_until scn.char_stream in
    String.of_seq (List.to_seq lst)

  let is_letter = function
    | 'a' .. 'z'
    | 'A' .. 'Z' -> true
    | _ -> false

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

  let is_xdigit = function
    | '0' .. '9'
    | 'a' .. 'f'
    | 'A' .. 'F' -> true
    | _ -> false

  let is_odigit = function
    | '0' .. '7' -> true
    | _ -> false

  let is_bidigt = function
    | '0' | '1' -> true
    | _ -> false

  let is_size_letter = function
    | 'L' | 'l' | 'u' | 'U' -> true
    | _ -> false

  let is_identifier c =
    is_letter c || is_digit c || c = '_'

  let is_integer c =
    is_digit c || is_size_letter c

  let is_xinteger c =
    is_xdigit c || is_size_letter c

  let is_ointeger c =
    is_odigit c || is_size_letter c

  let is_binteger c =
    is_bidigit c || is_size_letter c

  let is_operator = function
    | '+' | '-'
    | '*' | '/'
    | '%' | '>'
    | '<' | '&'
    | '|' | '='
    | '!' | '~' -> true
    | _ -> false

  let assess_keyword lexeme =
    match lexeme with
    | "auto" | "break"
    | "if"   | "inline"
    | "unsigned" | "void"
    | "case" | "int"
    | "volatile" | "char" 
    | "long" | "while"
    | "const" | "register"
    | "_Alignas" | "continue"
    | "restrict" | "_Alignof"
    | "default" | "return"
    | "_Atomic" | "do"
    | "short" | "_Bool"
    | "double" | "signed"
    | "_Complex" | "else"
    | "sizeof" | "_Generic"
    | "enum" | "static"
    | "_Imaginary" | "extern"
    | "extern" | "struct"
    | "_Noreturn" | "float"
    | "switch" | "_Static_assert"
    | "for" | "typedef"
    | "_Thread_local" | "goto"
    | "union" -> Keyword lexeme
    | _ -> Identifier lexeme

  let rec scan scn =
    match Stream.peek scn.char_stream with
    | 'U' | 'L' ->
      let npeek_lst = Stream.npeek 2 scn.char_stream in
      if npeek_lst.[1] = '"' || npeek_lst.[1] = '\''
      then scan (scn ++ EncodingPrefix (String.make 1 (Stream.next scn.char_stream)))
      else scan scn
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> 
      scan (scn ++ Identifier ((implode_while is_identifier scn) |> assess_keyword))
    | '1' .. '9' ->
      scan (scn ++ IntegerLiteral (implode_while is_integer scn))
    | '+' | '-' | '>' | '<' | '=' | '*' | '%' | '/' | '&' | '|' | '!' | '~' ->
      scan (scn ++ Operator (implode_while is_operator scn.))
    | '0' ->
      begin
        Stream.skip scn.char_stream;
        let demarque = Stream.next scn.char_stream in
        match demarque with
        | 'x' | 'X' -> (scan ++ HexLiteral (implode_while is_xinteger))
        | 'o' | 'O' -> (scan ++ OctLiteral (implode_while is_ointeger))
        | 'b' | 'B' -> (scan ++ BinLiteral (implode_while is_binteger))
        | _ -> raise (Wrong_symbol of (demarque, (Stream.position scn.char_stream)))
      end
    | '"' ->
      scan (scn ++ StringLiteral (implode_until ((=)'"') ((>>)scn)))
    | '\'' ->
      scan (scn ++ CharacterLiteral (implode_until ((=)'\'') ((>>)scn)))
    | exception Empty_stream ->
      if Stream.is_empty scn.token_stream
      then raise Blank_file
      else if (Stream.peek_last scn.token_stream
               |> is_valid_end)
      then raise End_of_input
      else raise (Unexpected_eof (Stream.position scn.char_stream))


end
