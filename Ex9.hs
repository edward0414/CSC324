{-# LANGUAGE InstanceSigs #-}
{-|
Module: Ex9
Description: Exercise 9: Working with Trees
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}
module Ex9
    ( Tree
    , treeSum
    , collectTreeState
    , collectTreeM
    )
where

-- <https://hackage.haskell.org/package/base/docs/Data-Foldable.html>
import qualified Data.Foldable as Foldable

-- <https://hackage.haskell.org/package/base/docs/Control-Monad.html>
-- (You may or may not want to use this.)
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State

import Ex9Types (Tree(..))

-- | Imports used for testing purposes only.
import Test.QuickCheck (quickCheck, (===), Property)

-------------------------------------------------------------------------------
-- |
-- = Task 1: The Tree data type
-------------------------------------------------------------------------------
-- | Returns the sum of the numbers in the tree.
getTreeValues :: Tree  -> [Integer]
getTreeValues Empty = Empty
getTreeValues (Tree a children) = Cons (a) (map getTreeValues children)

treeSum :: Tree Integer -> Integer
treeSum Null a = 0
treeSum (Tree a children) = a + foldl (\treeVal total -> treeVal + total) 0 children

-- | Make Tree an instance of Functor.
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap = 


---------------------------------------------------------------------------------
-- |
-- = Task 2: A new type class
---------------------------------------------------------------------------------
-- | Foldable type class. Only @foldr@ is necessary to implement.
-- For the complete documentation of this typeclass, see
-- <https://hackage.haskell.org/package/base/docs/Data-Foldable.html>.
instance Foldable Tree where
    -- HINT: the "b" value is the "accummulator".
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f x (Tree root subtrees) = undefined


---------------------------------------------------------------------------------
-- |
-- = Task 3: Trees, State, and monads
---------------------------------------------------------------------------------
-- You can use built-in functions imported from Control.Monad here,
-- and copy over work from Lab 10 (where you did something similar for lists).
collectTreeState :: Tree (State.State s a) -> State.State s (Tree a)
collectTreeMaybe = undefined

collectTreeM :: Monad m => Tree (m a) -> m (Tree a)
collectTreeM = undefined


---------------------------------------------------------------------------------
-- |
-- = Sample tests
---------------------------------------------------------------------------------
sampleTree :: Tree Integer
sampleTree = Tree
    5
    [ Tree 3    []
    , Tree 6    [Tree 4 [], Tree 10 []]
    , Tree 7    [Tree 15 [Tree 99 []]]
    , Tree (-1) []
    ]


test_treeFunctor_id :: Tree Integer -> Property
test_treeFunctor_id t = fmap id t === t

-- Uses the "sum" function from Foldable, which has a default implementation
-- in terms of Foldable.foldr that you should implement for Tree.
test_sample_FoldableSum :: Property
test_sample_FoldableSum =
    Foldable.sum sampleTree === 5 + 3 + 6 + 4 + 10 + 7 + 15 + 99 - 1

-- Test relationship between Foldable.sum and Foldable.length.
test_treeSumAndLength :: Tree Integer -> Property
test_treeSumAndLength t = Foldable.length t === Foldable.sum (fmap (\_ -> 1) t)


-- | Helper to increment an integer counter state.
incr :: State.State Integer Integer
incr = State.state $ \s0 -> (s0, s0 + 1)

-- | This test shows how to implement a preorder numbering of a tree
-- using @collectTreeState@. This is related to the implementation
-- of a postorder traversal shown in the course notes.
test_sample_postorder :: Property
test_sample_postorder =
    let
        postOrderState = collectTreeState $ fmap (\_ -> incr) sampleTree
        (tree, _)      = State.runState postOrderState 0
    in tree === Tree
        0
        [ Tree 1 []
        , Tree 2 [Tree 3 [], Tree 4 []]
        , Tree 5 [Tree 6 [Tree 7 []]]
        , Tree 8 []
        ]

-- | Repeat of previous test, except using collectTreeM instead.
test_sample_postorderM :: Property
test_sample_postorderM =
    let
        postOrderState = collectTreeM $ fmap (\_ -> incr) sampleTree
        (tree, _)      = State.runState postOrderState 0
    in tree === Tree
        0
        [ Tree 1 []
        , Tree 2 [Tree 3 [], Tree 4 []]
        , Tree 5 [Tree 6 [Tree 7 []]]
        , Tree 8 []
        ]

-- Test that collectTreeM works when given another monad (Maybe).
test_collectM_onlyJusts :: Tree Integer -> Property
test_collectM_onlyJusts t = collectTreeM (fmap Just t) === Just t


main :: IO ()
main = do
    quickCheck test_sample_FoldableSum
    quickCheck test_treeFunctor_id
    quickCheck test_treeSumAndLength
    quickCheck test_sample_postorder
    quickCheck test_sample_postorderM
    quickCheck test_collectM_onlyJusts