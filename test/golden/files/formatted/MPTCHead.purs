module MPTCHead where

instance patchTuple ∷
  ( Patch a da
  , Patch b db
  ) =>
  Patch (Tuple a da) (Tuple b db)
