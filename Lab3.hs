{-|
Module: Lab3
Description: Lab 3: A Simple Interpreter
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

Lab handout: https://www.cs.toronto.edu/~david/csc324/labs/lab3/handout.html.
-}

module Lab3 (Op, ArithExpr, interpretArithWithIds) where


    -- Imports the Data.Map library, to be used to represent the environment.
    -- See <http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html>
    -- for details.
    -- Note that this is a qualified import, meaning you'll need to prefix all
    -- identifiers imported from this library with "Map.", e.g. Map.findWithDefault.
    import qualified Data.Map.Strict as Map
    
    
    data ArithExpr
        = Number Integer                 -- ^ A numeric literal
        | BinOp ArithExpr Op ArithExpr   -- ^ A binary operation
        | Identifier String              -- ^ An identifier name (Task 2)
        deriving (Show, Eq)
    
    data Op = Plus | Times deriving (Show, Eq)
    
    
    -- |
    -- == Examples of expressions
    
    -- | Example: The number 20.
    numberExample :: Expr
    numberExample = Number 20
    
    -- | Example: (20 + 30)
    plusExample :: Expr
    plusExample = BinOp (Number 20) Plus (Number 30)
    
    -- | Example: (a + b)
    timesWithIdExample :: Expr
    timesWithIdExample = BinOp (Identifier "a") Times (Identifier "b")
    
    
    -- | Returns the value of the given arithmetic expression,
    -- under the given environment. Remember that you may assume that
    -- every identifier in the expression is actually in the environment.
    -- This means you can use Map.findWithDefault and give a dummy default value
    -- for this exercise. (We'll discuss robust error-handling later in the course).
    -- Note: you'll need to replace our given stub with pattern-matching rules;
    -- See Exercise 2 for a refresher on how these patterns should be written.
    --
    -- Tip for testing:
    -- You can use Map.fromList [(key1, value1), (key2, value2), ...] to create a Map.
    interpretArithWithIds :: Map.Map String Int -> ArithExpr -> Int
    interpretArithWithIds env expr = undefined