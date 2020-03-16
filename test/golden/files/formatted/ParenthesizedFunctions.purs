module ParenthesizedFunctions where

x ∷ (Int -> Int) -> Int
x f = f 1

y ∷
  ∀ expected r unexpected.
  (∀ a. Show a => a -> { expected ∷ expected, unexpected ∷ unexpected | r }) ->
  Boolean ->
  { expected ∷ expected, unexpected ∷ unexpected }
y f x = { expected, unexpected }
  where
  expected = result.expected
  result = f x
  unexpected = result.unexpected
