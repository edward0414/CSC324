-- CSC324 Fall 2019: Example Code
import qualified Control.Monad.State as State

{- For reference, a naive mutating implementation in Python:
count = 0

def fib_counted(n):
    global count
    count += 1

    if n < 2:
        return n
    else:
        return fib(n - 1) + fib(n - 2)
-}


-- | Standard Fibonacci implementation.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n =
    let f1 = fib (n - 1)
        f2 = fib (n - 2)
    in
        f1 + f2

-- | Fibonacci with an accumulated counter.
fibCounted :: Integer -> Integer -> (Integer, Integer)
fibCounted 0 count = (0, count + 1)
fibCounted 1 count = (1, count + 1)
fibCounted n count =
    let (f1, count1) = fibCounted (n - 1) count
        (f2, count2) = fibCounted (n - 2) count1
    in
        (f1 + f2, count2 + 1)

-------------------------------------------------------------------------------
-- Core state implementation (built-in, but shown for educational purposes)
-------------------------------------------------------------------------------
data State s a = State (s -> (a, s))

get :: State s s
get = State $ \state -> (state, state)

put :: s -> State s ()
put x = State $ \_ -> ((), x)

runState :: State s a -> s -> (a, s)
runState (State f) init = f init


-- | Fibonacci with a "mutable state" counter
fibCountedS :: Integer -> State Integer Integer
fibCountedS 0 = State $ \count -> (0, count + 1)
fibCountedS 1 = State $ \count -> (1, count + 1)
fibCountedS n = State $ \count0 ->
    let (f1, count1) = runState (fibCountedS (n - 1)) count0
        (f2, count2) = runState (fibCountedS (n - 2)) count1
        (f3, count3) = runState (State $ \c -> (f1 + f2, c + 1)) count2
    in
        (f3, count3)


-------------------------------------------------------------------------------
-- Chaining stateful operations
-------------------------------------------------------------------------------
andThen :: State s a -> State s b -> State s b
andThen stateOp1 stateOp2 = State $ \state0 ->
    let (x1, state1) = runState stateOp1 state0
        (x2, state2) = runState stateOp2 state1
    in
        (x2, state2)

bind :: State s a -> (a -> State s b) -> State s b
bind stateOp1 opMaker = State $ \state0 ->
    let (x1, state1) = runState stateOp1 state0
        (x2, state2) = runState (opMaker x1) state1
    in
        (x2, state2)

-- | Fibonacci with a "mutable state" counter, chained version.
-- Note the removal of all manual "threading" of state.
fibCountedD :: Integer -> State Integer Integer
fibCountedD 0 = State (\count -> (0, count + 1))
fibCountedD 1 = State (\count -> (1, count + 1))
fibCountedD n =
    (fibCountedD (n - 1)) `bind` \f1 ->
    (fibCountedD (n - 2)) `bind` \f2 ->
    (State $ \c -> (f1 + f2, c + 1))


-------------------------------------------------------------------------------
-- Built-in State and monadic operations
-------------------------------------------------------------------------------

-- | Fibonacci with a "mutable state" counter, using monadic operations.
fibCountedM :: Integer -> State.State Integer Integer
fibCountedM 0 = State.state $ \count -> (0, count + 1)
fibCountedM 1 = State.state $ \count -> (1, count + 1)
fibCountedM n = do
    f1 <- fibCountedM (n - 1)
    f2 <- fibCountedM (n - 2)
    State.state $ \c -> (f1 + f2, c + 1)

    -- Remember, do notation is just a macro; the expanded form is
    -- fibCountedM (n - 1) >>= \f1 ->
    -- fibCountedM (n - 2) >>= \f2 ->
    -- State.state $ \c -> (f1 + f2, c + 1)

-- | Final and cleanest version of the fibCounted. This changes two things
-- from fibCountedM: first, it abstracts the "count + 1" operation out of the
-- three individual cases (this is what the Python version does), and second
-- it makes use of the other monadic function @return@, which for State
-- has the following implementation:
--
--     return :: a -> State s a
--     return x = State.state $ \c -> (x, c)
--
-- From the code below you can probably intuit why this function called "return",
-- though it is just a function, and not a keyword like in every other language.
fibCountedFinal :: Integer -> State.State Integer Integer
fibCountedFinal n = do
    _ <- increment
    -- Note: the whole `if` expression is counted as a single expression in
    -- the do notation, even though it's split across multiple lines.
    if n < 2
    then
        return n
    else do
        f1 <- fibCountedFinal (n - 1)
        f2 <- fibCountedFinal (n - 2)
        return (f1 + f2)

increment :: State.State Integer ()
increment = do
    curr <- State.get
    State.put (curr + 1)