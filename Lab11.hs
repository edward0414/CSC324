{-|
Module: Lab11
Description: Lab 11: Basic Type Checking, with Error-Checking
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

Lab handout: https://www.cs.toronto.edu/~david/csc324/labs/lab11/handout.html.
-}
module Lab11
    (typeCheck) 
where 
import Test.QuickCheck (quickCheck)

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
typeCheck (IntLiteral body) = Just (Integer_)
typeCheck (BoolLiteral body) = Just (Bool_)
typeCheck (Call Plus body1 body2) =
    let exp1 = typeCheck body1
        exp2 = typeCheck body2
    in
        if exp1 == exp2 && exp1 == Just (Integer_) then Just (Integer_) else Nothing

typeCheck (Call Equal body1 body2) =
    let exp1 = typeCheck body1
        exp2 = typeCheck body2
    in
        if exp1 == exp2 && exp1 == Just (Bool_) then Just (Bool_) else Nothing

typeCheck (If body1 body2 body3) =
    let exp1 = typeCheck body1
        exp2 = typeCheck body2
        exp3 = typeCheck body3
    in
        if exp1 == Just (Bool_) && exp2 == exp3 then exp2 else Nothing

main :: IO ()
main = do
    print (typeCheck (IntLiteral 3))
    print (typeCheck (BoolLiteral True))
    print (typeCheck (Call Equal (BoolLiteral True) (BoolLiteral True)))
    print (typeCheck (Call Equal (BoolLiteral True) (IntLiteral 3)))
    print (typeCheck (If (BoolLiteral True) (BoolLiteral True) (BoolLiteral True)))