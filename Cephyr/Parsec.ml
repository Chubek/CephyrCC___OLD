module Parsec = struct
  type 'a t =
    | SatisfySingle of 'a
    | SatisfyPair of ('a * 'a)
    | SatisfyMultiple of 'a list
    | SatisfyReturn of ('a -> t)
    | SatisfyBind of ('a -> 'a)
    | SatisfyPredicate of ('a -> bool)
    | Subsequent of t * t
    | Alternate of t * t
    | Many of t
    | Terminates of t * t
    | SepBy of t * t
    | EndBy of t * t
    | WrappedBy of t * t
    | Between of (t * t) * t
    | Prefix of t * t
    | Suffix of t * t
    | RightInfix of t * t
    | LeftInfix of t * t
    | Exactly of t * int
    | Range of t * (int * int)
    | Nested of t
    | AtLeastOnce of t
    | AtMostOnce of t
    | Mandatory of t
    | Optional of t


end
