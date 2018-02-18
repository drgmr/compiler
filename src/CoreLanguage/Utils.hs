module CoreLanguage.Utils
  ( isAtomicExpression,
    bindersOf,
    rhssOf,

    recursive,
    nonRecursive,

    mkMultiApplication
  ) where

import CoreLanguage.Types

-- General utilities

-- Checks if an Expression can be considered atomic
isAtomicExpression :: Expression a -> Bool
isAtomicExpression (Variable a) = True
isAtomicExpression (Number a) = True
isAtomicExpression e = False

-- Get the binders of a list of definitions
bindersOf :: [(a, b)] -> [a]
bindersOf xs = [name | (name, rhs) <- xs]

-- Get the right hand sides of a list of definitions
rhssOf :: [(a, b)] -> [b]
rhssOf xs = [rhs | (name, rhs) <- xs]

-- Expression utilities

-- Utility to mark a let(rec) as recursive
recursive :: IsRecursive
recursive = True

-- Utility to mark a let(rec) as non-recursive
nonRecursive :: IsRecursive
nonRecursive = False

-- Builds an expression that is the application of e2 to e1 n times
mkMultiApplication :: Int -> CoreExpression -> CoreExpression -> CoreExpression
mkMultiApplication n e1 e2 = foldl Application e1 (take n e2s)
  where e2s = e2 : e2s
