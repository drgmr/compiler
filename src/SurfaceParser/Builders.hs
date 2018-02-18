module SurfaceParser.Builders
    ( satisfies,
      literal,
      empty,
      alternative,
      chain,
      chain3,
      chain4,
      zeroOrMore,
      oneOrMore,
      oneOrMoreWithSeparator,
      apply,
      skip
    ) where

import SurfaceParser.Types (Parser)
import Lexer (keywords)

-- Creates a parser that succeeds if the lexeme satisfies the given function
satisfies :: (String -> Bool) -> Parser String
satisfies filter ((position, token):rest) = [ (token, rest) | filter token ]
satisfies filter []                       = []

-- Creates a parser for a given literal
literal :: String -> Parser String
literal string = satisfies (== string)

-- Creates a parser that always succeeds
empty :: a -> Parser a
empty result tokens = [(result, tokens)]

-- Creates a parser that is the "or" combination of two other parsers
alternative :: Parser a -> Parser a -> Parser a
alternative a b tokens = a tokens ++ b tokens

-- Creates a parser that is the chaining of two other parsers
chain :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
chain combine parser1 parser2 tokens =
    [ (combine lexeme1 lexeme2, rest')
    | (lexeme1, rest ) <- parser1 tokens
    , (lexeme2, rest') <- parser2 rest
    ]

-- Creates a parser that is the chaining of three other parsers
chain3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
chain3 combine parser1 parser2 parser3 tokens =
    [ (combine lexeme1 lexeme2 lexeme3, rest'')
    | (lexeme1, rest  ) <- parser1 tokens
    , (lexeme2, rest' ) <- parser2 rest
    , (lexeme3, rest'') <- parser3 rest'
    ]

-- Creates a parser that is the chaining of four other parsers
chain4
    :: (a -> b -> c -> d -> e)
    -> Parser a
    -> Parser b
    -> Parser c
    -> Parser d
    -> Parser e
chain4 combine parser1 parser2 parser3 parser4 tokens =
    [ (combine lexeme1 lexeme2 lexeme3 lexeme4, rest4)
    | (lexeme1, rest1) <- parser1 tokens
    , (lexeme2, rest2) <- parser2 rest1
    , (lexeme3, rest3) <- parser3 rest2
    , (lexeme4, rest4) <- parser4 rest3
    ]

-- Creates a parser that requires zero or more ocurrences of its lexeme
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser = oneOrMore parser `alternative` empty []

-- Creates a parser that requires one or more ocurrences of its lexeme
oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = chain (:) parser (zeroOrMore parser)

-- Creates a parser that requires one or more ocurrences of its lexeme interleaved with a separator
oneOrMoreWithSeparator :: Parser a -> Parser b -> Parser [a]
oneOrMoreWithSeparator parser separator =
    chain (:) parser continuation
    where 
        continuation = zeroOrMore (chain (\a b -> b) separator parser)
    -- zeroOrMore $ chain const parser separator

-- Transforms a parser by applying a given function
apply :: Parser a -> (a -> b) -> Parser b
apply parser mapper tokens =
    [ (mapper value, tokens') | (value, tokens') <- parser tokens ]

-- Throws away the result of the first parser
skip :: Parser a -> Parser b -> Parser b
skip = chain (\a b -> b)
