module HLInter = struct
   type t =
           { prog   : stmt Stream.t
           ; lvtbl  : (string, lvalue) Hashtbl.t
           ; typtbl : (string, typ) Hashtbl.t
           }
 
   and lvalue =
           { lbase  : lbase
           ; loffs  : loffs
           }

   and lbase =
           | Var of variable
           | Mem of exp

   and loffs =
           | NoOffs
           | Field of field * loffs 
           | Index of exp * loffs

   and exp =
           | Constant of const
           | Lvalue of lvalue
           | SizeOfExp of exp
           | SizeOfType of typ
           | AlignOfType of typ
           | UnOp of unop * exp
           | BinOp of binop * exp
           | Cast of typ * exp
           | AddrOf of lvalue
           | StartOf of lvalue

   and instr =
           | Set of lvalue * exp
           | Call of lvalue option * exp list
           | Asm of string * lvalue list * exp list

   and stmt =
           | Instr of instr list
           | Return of exp option
           | Goto of stmt
           | Break
           | Continue
           | If of exp * stmt list * stmt list
           | Switch of exp * stmt list * stmt list
           | Loop of stmt list

   and typ =
           | Void
           | Array of typ * exp list
           | Int of int_kind
           | Float of float_kind
           | Ptr of typ
           | Fun of typ * variable list
           | Enum of string * item list
           | Named of string * typ
           | Struct of string * field list
           | Union of string * field list

   let empty : t = { prog   = Stream.empty () 
                   ; lvtbl  = Hashtbl.create 21 
                   ; typtbl = Hashtbl.create 21 
                   }
end
