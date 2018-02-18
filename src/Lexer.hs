module Lexer
  ( Position,
    Token,

    lex,
    twoCharOperators,
    isIdChar,
    keywords
  ) where

import Prelude hiding (lex)
import Data.Char (isAlpha, isDigit, isSpace)

import CoreLanguage.Types (CoreProgram)

-- The line and column of a lexeme
type Position = (Int, Int)

-- A language token with its line number and column
type Token = (Position, String)

-- TODO(edupc): is this useful at all?
-- parse :: String -> CoreProgram
-- parse = syntax . lex

-- Splits a given string into a list of tokens
lex :: String -> [Token]
lex = lex' 1 0

-- Splits a given string into a list of tokens, keeping track of the current line number and column
lex' :: Int -> Int -> String -> [Token]
lex' line column [] = []
lex' line column chars =
  case chars of
    -- Whitespace is ignored
    (c:cs) | isSpace c -> lex' line (column + 1) cs
    -- New lines increment the counter
    ('\n':cs) -> lex' (line + 1) column cs
    -- Single line comments are ignored until end of line
    ('#':cs) ->
      let rest = dropWhile (/= '\n') cs
      in lex' line column rest
    -- Check for two char operators and handle them as a single token
    (a:b:cs) | [a, b] `elem` twoCharOperators ->
      ((line, column), [a, b]) : lex' line (column + 2) cs
    -- Check for multi digit literals
    (c:cs) | isDigit c ->
        let 
          numToken = c : takeWhile isDigit cs
          rest = dropWhile isDigit cs
        in ((line, column), numToken) : lex' line (column + length numToken) rest
    -- Check for alphanumeric identifiers
    (c:cs)
      | isAlpha c ->
        let
          varToken = c : takeWhile isIdChar cs
          rest = dropWhile isAlpha cs
        in ((line, column), varToken) : lex' line (column + length varToken) rest
    -- Anything else goes as is
    (c:cs) -> ((line, column), [c]) : lex' line (column + 1) cs

twoCharOperators :: [String]
twoCharOperators = ["==", "~=", ">=", "<=", "->"]

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]