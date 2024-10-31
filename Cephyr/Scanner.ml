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
    | CharacterLiteral of char
    | IntegerLiteral of int64
    | BooleanLiteral of bool
    | LeftDelimiter of char
    | RightDelimiter of char
    | Encoding of string
    | EncodingPrefix of string
    | SizeSuffix of string
    | SeparatorPunctuation of char

  exception Blank_file
  exception End_of_input
  exception Wrong_symbol of char
  exception Unexpected_eof of int

  let (++) scn tok =
    Stream.append scn.token_stream [tok]

  let implode_while pred scn =
    let lst = Stream.take_while pred scn.char_stream in
    String.of_seq (List.to_seq lst)

  let implode_until pred scn =
    let lst = Stream.take_until scn.char_stream in
    String.of_seq (List.to_seq lst)

  let rec scan scn =
    match Stream.peek scn.char_stream with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> 
      scan (scn ++ ((implode_while is_identifier scn) |> assess_keyword))
    | exception Empty_stream ->
      if Stream.is_empty scn.token_stream
      then raise Blank_file
      else if (Stream.peek_last scn.token_stream
               |> is_valid_end)
      then raise End_of_input
      else raise (Unexpected_eof (Stream.position scn.char_stream))


end
