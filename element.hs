module Element where

data Value =
  I Integer | B Bool

instance Show Value where
  show (I i) = show i
  show (B b) = show b

(+) :: Value -> Value -> Value
(+) (I x) (I y) = (I (x Prelude.+ y) )
(+) _ _ = error "Run-time error"

(*) :: Value -> Value -> Value
(*) (I x) (I y) = (I (x Prelude.* y) )
(*) _ _ = error "Run-time error"

(-) :: Value -> Value -> Value
(-) (I x) (I y) = (I (x Prelude.- y) )
(-) _ _ = error "Run-time error"

(==) :: Value -> Value -> Value
(==) (I x) (I y) = (B (x Prelude.== y) )
(==) (B x) (B y) = (B (x Prelude.== y) )
(==) _ _ = error "Run-time error"

(<=) :: Value -> Value -> Value
(<=) (I x) (I y) = (B (x Prelude.<= y) )
(<=) _ _ = error "Run-time error"

(&&) :: Value -> Value -> Value
(&&) (B x) (B y) = (B (x Prelude.&& y) )
(&&) _ _ = error "Run-time error"

not :: Value -> Value
not (B x) = (B (Prelude.not x) )
not _ = error "Run-time error"