module Element where

-- Definition of the Value data type representing integer (I) and boolean (B) values.
data Value =
  I Integer | B Bool

instance Show Value where
  show (I i) = show i
  show (B b) = show b

-- Adds two integer values or raises a run-time error for non-integer values.
(+) :: Value -> Value -> Value
(+) (I x) (I y) = I (x Prelude.+ y)
(+) _ _ = error "Run-time error"

-- Multiplies two integer values or raises a run-time error for non-integer values.
(*) :: Value -> Value -> Value
(*) (I x) (I y) = I (x Prelude.* y)
(*) _ _ = error "Run-time error"

-- Subtracts two integer values or raises a run-time error for non-integer values.
(-) :: Value -> Value -> Value
(-) (I x) (I y) = I (x Prelude.- y)
(-) _ _ = error "Run-time error"

-- Compares two integer values or two boolean values, or raises a run-time error for different types.
(==) :: Value -> Value -> Value
(==) (I x) (I y) = B (x Prelude.== y)
(==) (B x) (B y) = B (x Prelude.== y)
(==) _ _ = error "Run-time error"

-- Less than or equal comparison between two integer values or raises a run-time error for non-integer values.
(<=) :: Value -> Value -> Value
(<=) (I x) (I y) = B (x Prelude.<= y)
(<=) _ _ = error "Run-time error"

-- Logical AND between two boolean values or raises a run-time error for non-boolean values.
(&&) :: Value -> Value -> Value
(&&) (B x) (B y) = B (x Prelude.&& y)
(&&) _ _ = error "Run-time error"

-- Logical NOT of a boolean value or raises a run-time error for non-boolean values.
not :: Value -> Value
not (B x) = B (Prelude.not x)
not _ = error "Run-time error"