module SurfaceParser.Parsers where

import Data.Char (isAlpha, isDigit)

import Lexer (Token, keywords, isIdChar)

import CoreLanguage 
  (Expression(..), 
   CoreProgram, 
   CoreExpression, 
   CoreSupercombinatorDefinition, 
   CoreAlternative, 
   Name, 
   recursive, 
   nonRecursive)

import SurfaceParser.Types (Parser)
import SurfaceParser.Builders 
  (satisfies, 
   literal,
   alternative, 
   empty, 
   chain, 
   chain3, 
   chain4, 
   zeroOrMore, 
   oneOrMore, 
   oneOrMoreWithSeparator, 
   apply, 
   skip)

-- The parser that identifies variable bindings
variable :: Parser String
variable = satisfies isVar
 where
  isVar cs | cs `elem` keywords = False
  isVar (c:cs) = isAlpha c && isVar' cs
  isVar' = foldr ((&&) . isIdChar) True

-- The parser that identifies number literals
number :: Parser Int
number = apply (satisfies isNumber) read
 where
  isNumber (c:cs) = isDigit c && isNumber' cs
  isNumber' = foldr ((&&) . isDigit) True

-- The main program parser
program :: Parser CoreProgram
program = oneOrMoreWithSeparator programSeparator (literal ";")

-- Parses tokens into a supercombinator definition
programSeparator :: Parser CoreSupercombinatorDefinition
programSeparator =
  let
    params = zeroOrMore variable
    body = literal "=" `skip` expression
  in chain3 combinator variable params body
  where combinator = (,,)

expression :: Parser CoreExpression
expression =
  letParser
    `alternative` caseParser
    `alternative` lambdaParser
    `alternative` applicationParser

letParser :: Parser CoreExpression
letParser =
  let
    keyword = literal "let" `alternative` literal "letrec"
    in' = literal "in"
  in chain4 combinator keyword definitions in' expression
  where
    combinator "let"    defs _ expr = Let nonRecursive defs expr
    combinator "letrec" defs _ expr = Let recursive defs expr

definitions :: Parser [(Name, CoreExpression)]
definitions = oneOrMoreWithSeparator definition (literal ";")

definition :: Parser (Name, CoreExpression)
definition = chain3 combinator variable (literal "=") expression
  where combinator name _ expr = (name, expr)

caseParser :: Parser CoreExpression
caseParser =
  let 
    keyword = literal "case" `skip` expression
    alternativeDefs = literal "of" `skip` caseAlternatives
  in chain combinator keyword alternativeDefs
  where combinator = Case

caseAlternatives :: Parser [CoreAlternative]
caseAlternatives = oneOrMoreWithSeparator caseAlternative (literal ";")

caseAlternative :: Parser CoreAlternative
caseAlternative =
  let
    index = literal "<" `skip` number
    bindings = literal ">" `skip` zeroOrMore variable
    rhs = literal "->" `skip` expression
  in chain3 combinator index bindings rhs
  where combinator = (,,)

lambdaParser :: Parser CoreExpression
lambdaParser =
  let
    bindings = literal "\\" `skip` oneOrMore variable
    body = literal "." `skip` expression
  in chain combinator bindings body
  where combinator = Lambda

applicationParser :: Parser CoreExpression
applicationParser = 
  apply (oneOrMore applicationExpressionParser) mkApplicationChain
  where mkApplicationChain = foldl1 Application

applicationExpressionParser :: Parser CoreExpression
applicationExpressionParser =
  numberParser
    `alternative` variableParser
    `alternative` constructorParser
    `alternative` parentesisParser

numberParser :: Parser CoreExpression
numberParser = apply number Number

variableParser :: Parser CoreExpression
variableParser = apply variable Variable

constructorParser :: Parser CoreExpression
constructorParser =
  let 
    typeTag = literal "Pack" `skip` literal "{" `skip` number
    variant = literal "," `skip` number
  in chain3 combinator typeTag variant (literal "}")
  where combinator a b _ = ConstructorTag a b

parentesisParser :: Parser CoreExpression
parentesisParser = 
  chain3 combinator (literal "(") expression (literal ")")
  where combinator _ exp _ = exp

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . program
 where
  takeFirstParse ((prog, []) : others) = prog
  takeFirstParse (parse : others) = takeFirstParse others
  takeFirstParse other = error "Syntax error"