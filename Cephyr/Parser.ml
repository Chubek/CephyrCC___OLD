module Parsec = struct
  type t = 
    { scanner   : Scanner.t
    ; absyn     : Absyn.t
    ; worklist  : string Stack.t 
    }

  let (<<-) prs i =
    Stream.append prs.absyn.top_level [i]

  let worklist_push prs lexeme =
    Stack.push lexeme prs.worklist

  let worklist_empty prs =
    Stack.is_empty prs.worklist

  let worklist_pop prs =
    Stack.pop prs.worklist

  let worklist_top prs =
    Stack.top prs.worklist

  let worklist_drop prs =
    let _ = worklist_pop prs in
    ()

  let worklist_pop_if_top pred prs =
    if pred (worklist_top prs)
    then 
      begin
        worklist_drop prs;
        true
      end
    else
      false

  let peek prs =
    Stream.peek prs.scanner.token_stream

  let next prs =
    Stream.next prs.scanner.token_strean

  let npeek prs n =
    Stream.npeek n prs.scanner.token_stream

  let peek_ahead prs n =
    Stream.peek_ahead n prs.scanner.token_strean

  let skip prs =
    Stream.skip prs.token_stream


  let rec parse prs =
    match next prs with
    | Keyword ("signed" | "unsigned") as tyw ->
      worklist_push tyw;
      parse prs
    | Keyword "void" ->
      worklist_push "void";
      parse prs
    | Keyword ("char" | "int" | "short" | "long") as tyw ->
      begin
        match peek prs with
        | Keyword ("double" | "long") when tyw = "long" ->
          worklist_push tyw;
          parse prs
        | _ ->
          let is_ref_type = worklist_pop_if_top ((=)"*") prs in
          let is_unsigned = worklist_pop_if_top ((=)"unsigned") prs in
          let is_signed = worklist_pop_if_top ((=)"signed") in


      end




end
