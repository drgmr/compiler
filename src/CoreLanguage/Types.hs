module CoreLanguage.Types
  ( Name,

    Expression(..),
    CoreExpression,

    CaseAlternative(..),
    CoreAlternative,

    IsRecursive,

    Program(..),
    CoreProgram,

    SupercombinatorDefinition(..),
    CoreSupercombinatorDefinition
  ) where

-- A specified name from the source code
type Name = String

-- |Represents an expression from the source code
data Expression a
  -- A variable binding with a name
  = Variable Name
  -- A number literal
  | Number Int
  -- A tag representing a call to a constructor with it's variant
  | ConstructorTag Int Int
  -- The application of one expression to another
  | Application (Expression a) (Expression a)
  -- A let expression with it's bindings and right hand side
  | Let IsRecursive [(a, Expression a)] (Expression a)
  -- A case expression with the expression to excrutinize and it's alternatives
  | Case (Expression a) [CaseAlternative a]
  -- A lambda expression with it's inputs
  | Lambda [a] (Expression a)
  deriving (Show)

-- The core language Expression type
type CoreExpression = Expression Name

-- An alternative in a Case Expression
-- Composed of a tag, a list of bindings and the right hand side expression
type CaseAlternative a = (Int, [a], Expression a)

-- The core language Alternative type
type CoreAlternative = CaseAlternative Name

-- Indicates if a let expression is recursive
type IsRecursive = Bool

-- The main AST type
type Program a = [SupercombinatorDefinition a]

-- The core type of Program
type CoreProgram = Program Name

-- Represents the definition of a supercombinator
type SupercombinatorDefinition a = (Name, [a], Expression a)

-- The core supercombination definition type
type CoreSupercombinatorDefinition = SupercombinatorDefinition Name
