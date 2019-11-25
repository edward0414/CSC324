{-|
Module: Lab11
Description: Lab 11: Basic Type Checking, with Error-Checking
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

Lab handout: https://www.cs.toronto.edu/~david/csc324/labs/lab11/handout.html.
-}
module Lab11 where

    data Expr = IntLiteral Integer
              | BoolLiteral Bool
              | Call Op Expr Expr    -- Function calls are always binary
              | If Expr Expr Expr
              deriving (Eq, Show)
    
    data Op = Plus | Equal deriving (Eq, Show)
    
    data Type = Integer_ | Bool_     -- Every expression has either Integer_ or Bool_ type.
              deriving (Eq, Show)
    
    
    -- | The type-checking function.
    -- Note the return type is @Maybe Type@ rather than just @Type@,
    -- to encode the possibility of failure if an expression does not have a valid type.
    typeCheck :: Expr -> Maybe Type
    typeCheck = undefined