module Absyn = struct
  type t = 
    { top_level  : top_level Stream.t
    ; entrypoint : entrypoint option
    }

  and top_level =
    | Type of typ * string option
    | GlobalVariable of vsig * rvalue option
    | Function of fsig * scope option

  and typ =
    { qual : typqual option
    ; kind : typkind
    ; tag  : string option
    }

  and typqual =
    | Const
    | Volatile
    | Restrict

  and typkind =
    | Void
    | Pointer of typ
    | Function of fsig
    | Array of typ * idx list
    | Struct of (string * typ) list
    | Union of (string * typ) list
    | Packed of typ list * (string * int) list
    | Enum of (string * int option) list
    | Float32
    | Float64
    | Float80
    | Uint8
    | Int8
    | Uint16
    | Int16
    | Uint32
    | Int32
    | Uint64
    | Int64
    | Char
    | UChar

  and fsig =
    { rettype : typ
    ; formals : (typ * string option) list
    ; vararg  : bool
    ; name    : string option
    ; storage : strgcls option
    ; inline  : bool
    }

  and vsig =
    { vname   : string
    ; vtype   : typ
    ; storage : strgcls option
    }

  and strgcls =
    | Extern
    | Static
    | Auto
    | Register

  and idx =
    | Minimum of exp
    | Exact of exp
    | NoIndex

  and infix_exp =
    { lop : exp
    ; rop : exp
    ; bop : bin_op
    }

  and prefix_exp =
    { oprand : exp
    ; uop    : unary_op
    }

  and postfix_exp =
    { operand : exp
    ; uop     : unary_op
    }

  and const_val =
    | String of string
    | Char of char
    | Integer of int64
    | Real of float
    | Ident of string

  and binary_op =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | ShiftBitsLeft
    | ShiftBitsRight
    | Conjunct
    | Disjunct
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | Equals
    | Unequals
    | GreaterThan
    | GreaterThanEquals
    | LesserThan
    | LesserThanEquals
    | Assignment of binary_op option

  and unary_op =
    | LogicalNot
    | BitwiseNot
    | IntegeralNegation
    | Increment
    | Decrement
    | AddressOf
    | Dereference
    | Membership of exp
    | Access of exp
    | Index of exp
    | Call of exp list

  and exp =
    | Constval of const_val
    | Prefix of prefix_exp
    | Postfix of postfix_exp
    | Infix of infix_exp

  and rvalue =
    | Basic of exp
    | Compound of init list

  and init =
    | DesignatedIndex of const_val * exp
    | DesignatedName of string * exp
    | Nested of rvalue

  and scope =
    { decls : (vsig * rvalue option) list
    ; stmts : stmt list
    }

  and stmt =
    | IfCond of if_guard * if_guard list * stmt option
    | ForLoop of for_guard * stmt
    | WhileLoop of exp * stmt
    | DoWhileLoop of stmt * exp
    | CaseSwitch of exp * (const_val * stmt) list * stmt
    | Goto of string
    | Return of exp
    | Break
    | Continue

  exception No_entrypoint

  let is_definition tl =
    match tl with
    | Tydecl (_, None) -> true
    | Gvdecl (_, None) -> true
    | Fndecl (_, None) -> true

  let get_entrypoint = function
    | { _ ; Some ep } -> ep
    | { _ ; None } -> raise No_entrypoint

end
