{-|
Module: Lab2
Description: Lab 2: Higher-Order Functions, and More List Practice
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

Lab handout: https://www.cs.toronto.edu/~david/csc324/labs/lab2/handout.html.
-}

module Lab2 (numPred, makeCounter) where

-- | We'll discuss the type signature in more detail later, but for now just
-- know that the @a@ is a /type variable/ that can be replaced by any type.
numPred :: (a -> Bool) -> [a] -> Int
numPred pred lst = undefined


makeCounter :: (a -> Bool) -> ([a] -> Int)
makeCounter pred = undefined

