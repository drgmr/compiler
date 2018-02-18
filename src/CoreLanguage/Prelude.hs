module CoreLanguage.Prelude
  ( preludeDefinitions
  ) where

import CoreLanguage.Types (CoreProgram, Expression (..))


-- |The core language prelude
preludeDefinitions :: CoreProgram
preludeDefinitions =
  [ ("I" , ["x"], Variable "x")
  , ("K" , ["x", "y"], Variable "x")
  , ("K1", ["x", "y"], Variable "y")
  , ("S", ["f", "g", "x"], (Variable "f" `Application` Variable "x") `Application` (Variable "g" `Application` Variable "x"))
  , ("compose", ["f", "g", "x"], Variable "f" `Application` (Variable "g" `Application` Variable "x"))
  , ("twice", ["f"], (Variable "compose" `Application` Variable "f") `Application` Variable "f")
  ]
