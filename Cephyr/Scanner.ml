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
    | EncodingPrefix of string
    | SizeSuffix of string
    | IntegerLiteral of int64
    | RealLiteral of float
    | BooleanLiteral of bool
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

  let implode_while' pred scn =
    let lexeme = implode_while pred scn in
    Stream.skip scn.char_stream; lexeme

  let implode_until' pred scn =
    let lexeme = implode_until pred scn in
    Stream.skip scn.char_stream; lexeme

  let is_whitespace = function
    | '\t' | '\space' | '\r' | '\n' -> true
    | _ -> false

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
    | 'L' | 'l' | 'u' | 'U' | 'f' | 'F' -> true
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

  let is_real_num c =
    is_digit c || c = '.'

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
    | "true" -> BooleanLiteral true
    | "false" -> BooleanLiteral false
    | _ -> Identifier lexeme

  let int_of_zlexeme lxm =
    lxm |> Int64.of_string

  let int_of_xlexeme lxm =
    ("0x" ^ lxm) |> Int64.of_string

  let int_of_olexeme lxm =
    ("0o" ^ lxm) |> Int64.of_string

  let int_of_blexeme lxm =
    ("0b" ^ lxm) |> Int64.of_string

  let real_of_rlexeme lxm =
    lxm |> Float.of_string

  let is_valid_end tok =
    match tok with
    | RightDelimiter '}' -> true
    | _ -> false

  let rec scan scn =
    match Stream.peek scn.char_stream with
    | '\space' | '\t' | '\r' | '\n' ->
      Stream.drop_while is_whitespace scn.char_stream;
      scan scn
    | ('L' | 'u' | 'U' | 'l' | 'f') as c ->
      begin
        match Stream.peek_last scn.token_stream with
        | IntegerLiteral _ | RealLiteral _ ->
          scan (scn ++ SizeSuffix (implode_while is_size_letter scn))
        | _ -> 
          let demarque = Stream.peek_ahead 1 scn.char_stream in
          match demarque with
          | '\'' | '"' ->
            Stream.skip scn.char_stream;
            scan (scn ++ EncodingPrefix (String.make c))
          | '8' ->
            let demarque' = Stream.peek_ahead 2 scn.char_stream in
            if demarque' = '\'' || demarque = '"'
            then 
              begin
                Stream.skip scn.char_stream;
                Stream.skip scn.char_stream;
                scan (scn ++ EncodingPrefix "u8")
              end
      end
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> 
      scan (scn ++ (implode_while is_identifier scn) |> assess_keyword)
    | '1' .. '9' ->
      begin
        let integer_lxm = Stream.take_while is_integer scn.char_stream in
        match Stream.peek scn.char_stream with
        | '.' ->
          let real_left = Stream.take_while is_real_num scn.char_stream in
          scan (scn ++ RealLiteral ((integer_lxm ^ real_left) |> real_of_rlexeme))
        | _ ->
          scan (scn ++ IntegerLiteral (integer_lxm |> int_of_zlexeme))
      end
    | '+' | '-' | '>' | '<' | '=' | '*' | '%' | '/' | '&' | '|' | '!' | '~' | '.' ->
      scan (scn ++ Operator (implode_while is_operator scn))
    | (';' | ',' | ':' | '?') as sep ->
      Stream.skip scn.char_stream;
      scan (scn ++ SeparatorPunctuaction sep)
    | ('(' | '{' | '[') as ldelim ->
      Stream.skip scn.char_stream;
      scan (scn ++ LeftDelimiter ldelim)
    | (')' | '}' | ']') as rdelim ->
      Stream.skip scn.char_stream;
      scan (scn ++ RightDelimiter rdelim)
    | '0' ->
      begin
        Stream.skip scn.char_stream;
        let demarque = Stream.next scn.char_stream in
        match demarque with
        | 'x' | 'X' -> (scan ++ IntegerLiteral ((implode_while is_xinteger) |> int_of_xlexeme))
        | 'o' | 'O' -> (scan ++ IntegerLiteral ((implode_while is_ointeger) |> int_of_olexeme))
        | 'b' | 'B' -> (scan ++ IntegerLiteral ((implode_while is_binteger) |> int_of_blexeme))
        | '0' .. '9' 
        | '.' ->  scan (scn ++ RealLiteral ((implode_while is_real_num) |> real_of_rlexeme))
        | _ -> raise (Wrong_symbol of (demarque, (Stream.position scn.char_stream)))
      end
    | '"' ->
      scan (scn ++ StringLiteral (implode_until' ((=)'"') ((>>)scn)))
    | '\'' ->
      scan (scn ++ CharacterLiteral (implode_until' ((=)'\'') ((>>)scn)))
    | exception Empty_stream ->
      begin
        if Stream.is_empty scn.token_stream
        then raise Blank_file
        else if (Stream.peek_last scn.token_stream
                 |> is_valid_end)
        then raise End_of_input
        else raise (Unexpected_eof (Stream.position scn.char_stream))
      end


end
