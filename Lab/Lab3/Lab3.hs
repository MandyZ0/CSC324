{- CSC324 Fall 2018: Lab 3 -}

-- The module definition line, including exports. Don't change this!
module Lab3 (Op, ArithExpr, interpretArithWithIds) where


-- Imports the Data.Map library, to be used to represent the environment.
-- See http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html
-- for details.
-- Note that this is a qualified import, meaning you'll need to prefix all
-- identifiers imported from this library with "Map.", e.g. Map.findWithDefault.
import qualified Data.Map.Strict as Map


data ArithExpr
    = Number Int                     -- ^ A numeric literal
    | Identifier String              -- ^ An identifier name
    | BinOp ArithExpr Op ArithExpr   -- ^ A binary operation
    deriving (Show, Eq)

data Op = Plus | Times deriving (Show, Eq)


-- |
-- == Examples of expressions

-- The number 20.
numberExpr = Number 20

-- (20 + 30)
plusExpr = BinOp (Number 20) Plus (Number 30)

-- (a * b)
timesExprWithId = BinOp (Identifier "a") Times (Identifier "b")


-- | Return the value of the given arithmetic expression,
-- under the given environment. Remember that you may assume that
-- every identifier in the expression is actually in the environment.
-- This means you can use Map.findWithDefault and give a dummy default value
-- for this exercise. (We'll discuss robust error-handling later in the course).
-- Note: you'll need to replace our given stub with pattern-matching rules!
--
-- You can use Map.fromList [(key1, value1), (key2, value2), ...] to create a map.
interpretArithWithIds :: Map.Map String Int -> ArithExpr -> Int
interpretArithWithIds env (Number n) = n
interpretArithWithIds env (Identifier id) = Map.findWithDefault 0 id env
interpretArithWithIds env (BinOp exp1 Plus exp2) = (interpretArithWithIds env exp1) + (interpretArithWithIds env exp2)
interpretArithWithIds env (BinOp exp1 Times exp2) = (interpretArithWithIds env exp1) * (interpretArithWithIds env exp2)