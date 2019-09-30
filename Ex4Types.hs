{-|
Module: Ex4Types
Description: Types for Exercise 4: Strictness Analysis
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

This module provides the public types required for the Haskell portion of
Exercise 4. You should review the data types carefully, but do not change
anything in this file! We will use a fresh copy of this file for testing purposes.
-}
module Ex4Types
    ( Prog(..)
    , FuncDef(..)
    , Expr(..)
    )
where
-------------------------------------------------------------------------------
-- * Task 1 (Expr)
-------------------------------------------------------------------------------
data Prog = Prog [FuncDef]     -- ^ A program is a list of function definitions.
          deriving (Show, Eq)

data FuncDef = FuncDef String [String] Expr  -- ^ Function name, parameters, and body.
             deriving (Show, Eq)

-- | This is the main "Expr" data type described in the handout.
-- Note that we use strings for identifiers, and so string "+" represents the
-- special "built-in" addition operation.
data Expr
  = Number Integer        -- ^ A numeric literal
  | Identifier String     -- ^ An identifier name
  | Call String [Expr]    -- ^ A function call
                          -- (first argument is the NAME of a function,
                          -- [Expr] is the argument list)
  deriving (Show, Eq)