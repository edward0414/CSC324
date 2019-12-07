{-|
Module: Ex8Types
Description: Types for Exercise 8: Representing Evaluation Order
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

This module provides the public types required for the Haskell portion of
Exercise 8. You should review the two data types (Binding and Expr) carefully,
but do not change anything in this file! We will use a fresh copy of this file
for testing purposes.
-}
module Ex8Types
    ( Binding(..)
    , Expr(..)
    )
where

data Expr
  = Number Integer
  | Identifier String
  | Call Expr [Expr]          -- ^ Function expression and argument subexpressions.
  deriving (Show, Eq)

data Binding = Binding String Expr deriving (Show, Eq)