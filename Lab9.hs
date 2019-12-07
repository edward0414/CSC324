{-|
Module: Lab9
Description: Lab 9: Polymorphism in Haskell
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

Lab handout: https://www.cs.toronto.edu/~david/csc324/labs/lab9/handout.html.
-}
module Lab9 where

    -------------------------------------------------------------------------------
    -- |
    -- = Task 1: Fitting some generic functions
    -------------------------------------------------------------------------------
    --
    -- For each of the functions below, give an implementation
    -- of them so that they are /total/ (always terminate and don't raise
    -- and error), and compile with the given type signatures.
    --
    -- Feel free to change the function and parameter names to be more descriptive.
    
    -- Note that (_, _) is the Haskell *tuple* type, which is also generically polymorphic.
    f0 :: a -> (a, a)
    f0 x = undefined
    
    f1 :: (a -> b -> c) -> a -> b -> c
    f1 x y z = undefined
    
    f2 :: (b -> c) -> (a -> b) -> a -> c
    f2 g h x = undefined
    
    -- What's special about this one?
    f3 :: (a, b) -> (c -> b)
    f3 (x, y) = undefined
    
    
    -------------------------------------------------------------------------------
    -- |
    -- = Task 2: One new generic type, `Maybe`
    -------------------------------------------------------------------------------
    
    -- For your reference, here's the definition of the `Maybe` type built into Haskell:
    -- data Maybe = Nothing | Just a
    
    safeHead :: [a] -> Maybe a
    safeHead = undefined
    
    safeTail :: [a] -> Maybe [a]
    safeTail = undefined
    
    onlyWhen :: (a -> Bool) -> a -> Maybe a
    onlyWhen = undefined
    
    try :: (a -> b) -> Maybe a -> Maybe b
    try = undefined
    
    
    -------------------------------------------------------------------------------
    -- |
    -- = Task 3: Introduction to typeclasses
    -------------------------------------------------------------------------------
    
    data Shape
        = Circle Float            -- ^ A circle with the given radius
        | Rectangle Float Float   -- ^ A rectangle with the given width and height
        | Square Float            -- ^ A square with the given side length