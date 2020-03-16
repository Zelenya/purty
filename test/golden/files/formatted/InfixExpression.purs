module InfixExpression where

instance ordNullable ∷ (Ord a) => Ord (Nullable a) where
  compare = compare `Fn.on` nullableToMaybe
