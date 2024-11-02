module Parsec = struct
  type t = 
    { scanner : Scanner.t
    ; absyn   : Absyn.t
    ; rules   : parse_rule list
    }

  and parse_rule =
    { input   : Stream.token list
    ; run     : parse_run
    ; success : unit -> unit
    ; failure : unit -> unit
    }

  and parse_run =
    | Satisfy of Stream.token list
    | Combine of parse_run * parse_run
    | Alternate of parse_run * parse_run
    | Many of parse_run * parse_run
    | Between of parse_run * parse_run
    | Skip of parse_run
    | SepBy of parse_run * parse_run
    | EndBy of parse_run * parse_run
    | Prefix of parse_run * parse_run
    | InfixLeft of parse_run * parse_run
    | InfixRight of parse_run * parse_run
    | Postfix of parse_run * parse_run
    | Range of parse_run * (int * int)
    | Exactly of parse_run * int
    | Nest of parse_run * parse_run

end
