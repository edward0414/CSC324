{-|
Module: Ex9Types
Description: Types for Exercise 9: Working with Trees
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

This module provides the public types required for Exercise 10. You should review
the data types carefully, but do not change anything in this file! We will use a
fresh copy of this file for testing purposes.
-}
module Ex9Types
    ( Tree(..)
    )
where

import Control.Monad (liftM2)
import Test.QuickCheck (oneof, sized, Arbitrary(..), Gen, resize, listOf)

-------------------------------------------------------------------------------
-- |
-- = Task 1: The Tree datatype
-------------------------------------------------------------------------------
-- | A recursive datatype for /non-empty/ trees with arbitrary branching.
data Tree a = Tree a [Tree a]
            deriving (Show, Eq)


-- | For testing, we first need to define how to generate "random" Trees.
-- You can safely ignore this code for this exercise.
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized tree'
      where
        tree' :: Arbitrary a => Int -> Gen (Tree a)
        tree' 0 = liftM2 Tree arbitrary (return [])
        tree' 1 = liftM2 Tree arbitrary (return [])
        tree' n = oneof
            [ liftM2 Tree arbitrary (return [])
            , liftM2
                Tree
                arbitrary
                (resize 3 (listOf (resize (n - 1) arbitrary)))
            ]