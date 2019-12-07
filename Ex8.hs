{-|
Module: Ex8
Description: Exercise 8: Representing Evaluation Order
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}
-- The module definition line, including exports. Don't change this!
module Ex8
    ( singleAssignmentForm
    , condensedSingleAssignmentForm
    )
where
import Test.QuickCheck (quickCheck)
import Ex8Types (Binding(..), Expr(..))

-------------------------------------------------------------------------------
-- |
-- * Task 1: Single assignment form
-------------------------------------------------------------------------------

-- | Returns the single assignment form of the given expression.
-- In Haskell, this is represented as a list of Bindings.
--
-- Technical hints:
-- 1. If you want to use an accumulator that stores more than one type of value,
--    you can either create your own data type or use the tuple data type,
--    e.g. (Integer, Bool).
-- 2. Use `show` to convert numbers into strings, to build the right identifiers
--    for the bindings. E.g., "_v" ++ (show 10).
-- 3. A good starting point is to define a main recursive helper with type
--    signature [Binding] -> Expr -> [Binding].
singleAssignmentForm :: Expr -> [Binding]
singleAssignmentForm = undefined

-------------------------------------------------------------------------------

prop_numeric_literal :: Bool
prop_numeric_literal =
    singleAssignmentForm (Number 5) == [Binding "_v0" (Number 5)]

prop_identifier :: Bool
prop_identifier =
    singleAssignmentForm (Identifier "x") == [Binding "_v0" (Identifier "x")]

prop_example_1 :: Bool
prop_example_1 =
    singleAssignmentForm
            (Call (Identifier "*") [Number 2, Number 3, Identifier "x"])
        == [ Binding "_v0" (Identifier "*")
           , Binding "_v1" (Number 2)
           , Binding "_v2" (Number 3)
           , Binding "_v3" (Identifier "x")
           , Binding
               "_v4"
               (Call
                   (Identifier "_v0")
                   [Identifier "_v1", Identifier "_v2", Identifier "_v3"]
               )
           ]

prop_example_2 :: Bool
prop_example_2 =
    singleAssignmentForm
            (Call
                (Identifier "+")
                [ Call (Identifier "*") [Number 2, Number 3]
                , Call (Identifier "*") [Number 6, Number 7]
                ]
            )
        == [ Binding "_v0" (Identifier "+")
           , Binding "_v1" (Identifier "*")
           , Binding "_v2" (Number 2)
           , Binding "_v3" (Number 3)
           , Binding
               "_v4"
               (Call (Identifier "_v1") [Identifier "_v2", Identifier "_v3"])
           , Binding "_v5" (Identifier "*")
           , Binding "_v6" (Number 6)
           , Binding "_v7" (Number 7)
           , Binding
               "_v8"
               (Call (Identifier "_v5") [Identifier "_v6", Identifier "_v7"])
           , Binding
               "_v9"
               (Call (Identifier "_v0") [Identifier "_v4", Identifier "_v8"])
           ]


-------------------------------------------------------------------------------
-- |
-- * Task 2: Condensed single assignment form
-------------------------------------------------------------------------------
-- | Returns the condensed single assignment form of the given expression.
-- *We will not test this function on an <expr> that is atomic (number/identifier).*
-- You may freely choose how to handle this case, which may depend on the rest of
-- your implementation.
condensedSingleAssignmentForm :: Expr -> [Binding]
condensedSingleAssignmentForm = undefined

-------------------------------------------------------------------------------

prop_condensed_example_1 :: Bool
prop_condensed_example_1 =
    condensedSingleAssignmentForm
            (Call (Identifier "*") [Number 2, Number 3, Identifier "x"])
        == [ Binding
                 "_v0"
                 (Call (Identifier "*") [Number 2, Number 3, Identifier "x"])
           ]

prop_condensed_example_2 :: Bool
prop_condensed_example_2 =
    condensedSingleAssignmentForm
            (Call
                (Identifier "+")
                [ Call (Identifier "*") [Number 2, Number 3]
                , Call (Identifier "*") [Number 6, Number 7]
                ]
            )
        == [ Binding "_v0" (Call (Identifier "*") [Number 2, Number 3])
           , Binding "_v1" (Call (Identifier "*") [Number 6, Number 7])
           , Binding
               "_v2"
               (Call (Identifier "+") [Identifier "_v0", Identifier "_v1"])
           ]

-------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_numeric_literal
    quickCheck prop_identifier
    quickCheck prop_example_1
    quickCheck prop_example_2
    quickCheck prop_condensed_example_1
    quickCheck prop_condensed_example_2