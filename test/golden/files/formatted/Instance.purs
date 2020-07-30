module Instance where

data Baz

instance fooBaz ∷ Foo Baz

instance barBaz ∷ Bar Baz where
  bar = append

derive instance eqBaz ∷ Eq Baz
derive instance showBaz ∷ Show Baz

newtype Bar = Bar Baz

derive newtype instance eqBar ∷ Eq Bar

instance quxCorGarIntBool ∷
  Qux
    Cor
    ( Gar
        Int
        Bool
    ) where
  qux = true
