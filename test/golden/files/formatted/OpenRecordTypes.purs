module OpenRecordTypes where

foo ∷
  ∀ x.
  {
  | x
  }
foo = ?foo

foreign import merge ∷ ∀ r1 r2 u. Union r1 r2 u => { | r1 } -> { | r2 } -> { | u }
