module Let where

import Prelude

shortLet = do
  let
    x = 12
  pure x

foo =
  let
    x ∷ Int
    x = 1
    y = 3
  in
    x

bar = do
  x <- pure 1
  let
    y = 3
    z ∷ Int
    z = 4
  pure (x + y + z)

baz =
  let
    x = 1
  in
    x
