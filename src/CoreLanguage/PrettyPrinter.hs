module CoreLanguage.PrettyPrinter
  -- ( display, prettyPrintProgram ) where
  where

import Prelude hiding (concat)

import CoreLanguage.Types
import CoreLanguage.Utils (isAtomicExpression)

-- An abstract representation of a core language code sequence
data CodeSequence
    -- The nil sequence - will be ignored
  = Nil
    -- A plain string
  | Str String
    -- The appendage of two sequences
  | Append CodeSequence CodeSequence
    -- Indicates that a sequence should be idented
  | Ident CodeSequence
    -- Prints out a new line, applying identation
  | Newline
  deriving (Show)

-- Creates a nil code sequence
nil :: CodeSequence
nil = Nil

-- Creates a code sequence indicating a new line is needed
newline :: CodeSequence
newline = Newline

-- Creates a code sequence from a given string
fromString :: String -> CodeSequence
fromString = Str

-- Creates a code sequence from a given number
fromNumber :: Int -> CodeSequence
fromNumber = Str . show

-- Creates a code sequence from a number adding padding to match a given width
fromNumberWithPadding :: Int -> Int -> CodeSequence
fromNumberWithPadding width n = fromString
  (space (width - length digits) ++ digits)
  where digits = show n

-- Wraps a code sequence into an identation marker
ident :: CodeSequence -> CodeSequence
ident = Ident

-- Appends two code sequences by creating an Append sequence
append :: CodeSequence -> CodeSequence -> CodeSequence
append = Append

-- Concatenates a list of sequences into one
concat :: [CodeSequence] -> CodeSequence
concat = foldl Append Nil

-- Concatenates a list of sequences into one, interleaving them with a specified sequence
interleave :: CodeSequence -> [CodeSequence] -> CodeSequence
interleave x = foldl (\a b -> Append (Append a b) x) Nil

-- Turn an Sequence into  a string
display :: CodeSequence -> String
display a = flatten 0 [(a, 0)]

-- Flattens a code sequence into a single string
flatten :: Int -> [(CodeSequence, Int)] -> String
flatten column [] = ""
flatten column ((Nil, ident) : xs) = flatten column xs
flatten column ((Str x, ident) : xs) = x ++ flatten column xs
flatten column ((Append x y, ident) : zs) =
  flatten column ((x, ident) : (y, ident) : zs)
flatten column ((Ident x, ident) : xs) = flatten column ((x, column + 2) : xs)
flatten column ((Newline, ident) : xs) = '\n' : space ident ++ flatten ident xs

-- Generates whitespace n times
space :: Int -> String
space n = replicate n ' '

-- Creates a code sequence from a given program
prettyPrintProgram :: CoreProgram -> CodeSequence
prettyPrintProgram defs =
  interleave
    (fromString ";" `append` newline)
    (map prettyPrintSupercombinatorDefinition defs)
    `append` newline

-- Creates a code sequence from a supercombinator definition
prettyPrintSupercombinatorDefinition :: CoreSupercombinatorDefinition -> CodeSequence
prettyPrintSupercombinatorDefinition (name, args, exp) =
  concat
    [ fromString name, fromString " "
    , interleave (fromString " ") (map fromString args)
    , fromString "= "
    , prettyPrintExpression exp
    ]

-- Creates a code sequence from a core expression
prettyPrintExpression :: CoreExpression -> CodeSequence
prettyPrintExpression (Variable v) = fromString v
prettyPrintExpression (Number i) = fromNumber i
prettyPrintExpression (Application f x) =
  prettyPrintExpression f
    `append` fromString " "
    `append` prettyPrintExpressionWithParens x
prettyPrintExpression (Let isrec defns expr) = concat
  [ fromString keyword
  , fromString " "
  , ident (prettyPrintDefinitions defns)
  , fromString "in "
  , prettyPrintExpression expr
  ]
  where keyword = if isrec then "letrec" else "let"
prettyPrintExpression (Case expr alters) = concat
  [ fromString "case "
  , prettyPrintExpression expr
  , fromString " of"
  , newline
  , prettyPrintAlternatives alters
  ]

-- Creates a code sequence from a list of definitions in a let expression
prettyPrintDefinitions :: [(Name, CoreExpression)] -> CodeSequence
prettyPrintDefinitions defns =
  interleave newline (map prettyPrintDefinition defns)

-- Creates a code sequence from a definition of a let expression
prettyPrintDefinition :: (Name, CoreExpression) -> CodeSequence
prettyPrintDefinition (name, expr) =
  concat [fromString name, fromString " = ", ident (prettyPrintExpression expr)]

-- Creates a code sequence from a list of alternatives of a case expression
prettyPrintAlternatives :: [CoreAlternative] -> CodeSequence
prettyPrintAlternatives alts = ident $ concat $ map prettyPrintAlternative alts

-- Creates a code sequence from an alternative of a case expression
prettyPrintAlternative :: CoreAlternative -> CodeSequence
prettyPrintAlternative (_index, bindings, expression) = concat
  [ interleave (fromString ", ") $ map fromString bindings
  , fromString " -> "
  , prettyPrintExpression expression
  , newline
  ]

-- Creates a code sequence surrounded by parentesis from a given expression
prettyPrintExpressionWithParens :: CoreExpression -> CodeSequence
prettyPrintExpressionWithParens exp
  | isAtomicExpression exp = prettyPrintExpression exp
  | otherwise = Str "(" `append` prettyPrintExpression exp `append` Str ")"
