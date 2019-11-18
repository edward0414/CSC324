{-|
Module: Lab10
Description: Lab 10: More on State and Monads
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

Lab handout: https://www.cs.toronto.edu/~david/csc324/labs/lab10/handout.html.
-}
module Lab10 where

    import Control.Monad.State.Lazy (State, state, runState)
    
    -------------------------------------------------------------------------------
    -- |
    -- = Task 1: A "mutable" stack
    -------------------------------------------------------------------------------
    -- | We use a list to represent a stack, where the front of the list
    -- represents the top of the stack. (`type` creates a *type synonym (or alias)*)
    type Stack a = [a]
    
    -- | A stateful operation representing a "pop" onto the stack.
    -- Can raise an error if the stack is empty.
    pop :: State (Stack a) a
    pop = state $ \stack -> (head stack, tail stack)
    
    -- | A stateful operation representing a "push" onto the stack.
    push :: a -> State (Stack a) ()
    push x = state $ \stack -> ((), x:stack)
    
    -- | A stateful operation that returns whether the stack is empty.
    -- Note that this function actually leaves the stack unchanged.
    isEmpty :: State (Stack a) Bool
    isEmpty = state $ \stack -> (null stack, stack)
    
    
    -- NOTE: for the functions below, implement them only in terms of the primitive
    -- stack operations found in the previous section.
    
    -- | A stateful operation that corresponds to the following imperative-style
    -- code snippet, where s is a mutable stack:
    --
    -- s.push(1)
    -- x = s.pop()
    -- if s.is_empty():
    --   s.push(x)
    -- else:
    --   s.push(x + 1)
    --
    -- The `if` is easier to deal with first if you're using explicit (>>=) rather
    -- than do notation!
    play :: State (Stack Integer) ()
    play = undefined
    
    -- | Remove and return the item that is second from the top of the stack.
    -- Assume that the stack has at least 2 items.
    removeSecond :: State (Stack a) a
    removeSecond = undefined
    
    -- | Remove and return the item that is third from the top of the stack.
    -- Assume that the stack has at least 3 items.
    removeThird :: State (Stack a) a
    removeThird = undefined
    
    -- | Remove and return the item that is n-th from the top of the stack.
    -- Assume that n >= 1, and that the stack has at least n items.
    removeNth :: Integer -> State (Stack a) a
    removeNth = undefined
    
    
    -------------------------------------------------------------------------------
    -- |
    -- = Task 2: More practice with State
    -------------------------------------------------------------------------------
    fmapState :: (a -> b) -> State s a -> State s b
    fmapState = undefined
    
    composeState :: (a -> State s b) -> (b -> State s c) -> (a -> State s c)
    composeState = undefined
    
    applyBinaryState :: (a -> b -> c) -> State s a -> State s b -> State s c
    applyBinaryState = undefined
    
    performAll_ :: [State s a] -> State s ()
    performAll_ = undefined
    
    performAll :: [State s a] -> State s [a]
    performAll = undefined
    
    -------------------------------------------------------------------------------
    -- |
    -- = Task 3: Generalizing to Monad
    -------------------------------------------------------------------------------
    composeM :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
    composeM = undefined
    
    applyBinaryM :: Monad m => (a -> b -> c) -> m a -> m b -> m c
    applyBinaryM = undefined
    
    collectM :: Monad m => [m a] -> m ()
    collectM = undefined
    
    collectM :: Monad m => [m a] -> m [a]
    collectM = undefined