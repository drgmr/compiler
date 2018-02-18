module SurfaceParser.Types 
    ( Parser
    ) where

import Lexer (Token)

-- Takes tokens and produces a lexeme alongside the remaining tokens
type Parser a = [Token] -> [(a, [Token])]