module Char where

foo ∷ Char -> Char
foo = case _ of
  'a' -> 'z'
  '\n' -> '\r'
  -- '\NUL' -> '\\'
  _ -> 'a'

bar ∷ Char
bar = '❌'

baz ∷ Char
baz = '\x2019'
