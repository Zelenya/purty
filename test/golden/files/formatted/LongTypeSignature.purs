module LongTypeSignature where

import Prelude
import Control.Monad.Eff
  ( Eff
  , kind Effect
  )
import Control.Monad.Eff.Console
  ( CONSOLE
  )

foreign import data FOO ∷ Effect

foreign import main_ ∷ ∀ e. Eff ( console ∷ CONSOLE, foo ∷ FOO | e ) Unit

main ∷
  ∀ e.
  Eff ( console ∷ CONSOLE, foo ∷ FOO | e ) Unit ->
  Eff ( console ∷ CONSOLE, foo ∷ FOO | e ) Unit
main _ = pure unit
