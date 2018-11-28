{- CSC324 Fall 2018: Assignment 2, Part 1

*Before starting, please review the exercise guidelines at
URL *
-}
module A2Core (
    State, emptyState,
    SusStream(SNull, SCons, SSuspended),
    (===), (&&&), (|||), conj, disj,
    fresh, streamToList, answer, run) where

-- Import data definitions and helper functions from the exercise code.
import Ex9 (Term(..), LVar(..), Substitution, unify, walkDeep)

import qualified Data.Map.Strict as Map
import Test.QuickCheck (quickCheck)

------------------------------------------------------------------------------
-- Data Definitions
------------------------------------------------------------------------------

-- | A tuple data type used to keep track of the logic variables generated when
-- solving a logic program.
-- The first element of the tuple represents the current substitution mapping,
-- while the second element is a counter representing the number of different
-- logic variables generated so far in solving the program.
type State = (Substitution, Int)

-- | A suspendable stream data type.
-- This is similar to the recursive list data type, except that there is a
-- new value constructor SSuspended that changes the order in which elements
-- appear when two of these streams are concatenated.
data SusStream a = SNull                      -- empty sequence
                 | SCons a (SusStream a)      -- standard first and rest
                 | SSuspended (SusStream a)   -- suspended SusStream
                 deriving (Show, Eq)

------------------------------------------------------------------------------
-- fresh: generating logic variables with a counter
------------------------------------------------------------------------------

-- | Generate `n` fresh LVars and pass them to `goalWithLVars` to produce a
-- new goal, and then return the goal.
-- Precondition: goalWithLVars expects exactly `n` LVars in its input list.
-- (Haskell's type system can't check this for us, unfortunately.)
fresh :: Int -> ([Term] -> (State -> SusStream State)) -> (State -> SusStream State)
fresh n goalWithLVars (sub, count) =
    let newLVars = map (TVar . LVar) [count..count + n - 1]
        newGoal = goalWithLVars newLVars
    in
        newGoal (sub, count + n)


------------------------------------------------------------------------------
-- The equality relation
------------------------------------------------------------------------------

-- | The relation (===) is a binary operation on terms, and will return
-- a **goal**, which is a function of type (State -> SusStream State).
-- Since you have already written the function `unify`, this function
-- should be fairly simple.
-- Returns a SusStream containing zero or one `State`, depending on whether
-- the `unify` call returned a success or failure.
(===) :: Term -> Term -> (State -> SusStream State)
u === v = \(sub, counter) ->
    undefined


------------------------------------------------------------------------------
-- Conjunctions, Disjunctions, and their stream helpers
--
-- You are not responsible for understanding the implementations of these functions.
------------------------------------------------------------------------------

-- | Binary conjunction.
-- goal1 is tried on the given state, and each resulting state is passed
-- to goal2. The final "SusStream State" contains the states that satisfy both goals.
(&&&) :: (State -> SusStream State) -> (State -> SusStream State) -> (State -> SusStream State)
goal1 &&& goal2 = \state -> sConcatMap goal2 (goal1 state)

-- | Binary disjunction.
-- The final "SusStream State" contains the states that satisfy goal1 or goal2.
-- Note that this is where the SSuspended constructor is used.
(|||) :: (State -> SusStream State) -> (State -> SusStream State) -> (State -> SusStream State)
goal1 ||| goal2 = \state -> sAppend (goal1 state) (SSuspended (goal2 state))

-- | Concatenate two suspended streams.
-- If the first SusStream has been suspended, the result is a new suspended SusStream
-- but with the SusStream order switched. (Essentially, the suspended elements
-- are moved to the back.)
-- You do not have to call this function yourself for this assignment.
sAppend :: SusStream a -> SusStream a -> SusStream a
sAppend SNull s2 = s2
sAppend (SCons x xs) s2 = SCons x (sAppend xs s2)
sAppend (SSuspended s1) s2 = SSuspended (sAppend s2 s1)  -- Note the order

-- | A version of concatMap for suspendable streams.
-- This is very similar to the concatMap definition for regular lists,
-- and is provided here because we use it as a helper for (&&&) above.
-- You do not have to call this function yourself for this assignment.
sConcatMap :: (a -> SusStream b) -> SusStream a -> SusStream b
sConcatMap f SNull = SNull
sConcatMap f (SCons x xs) = sAppend (f x) (sConcatMap f xs)
sConcatMap f (SSuspended stream) = SSuspended (sConcatMap f stream)


------------------------------------------------------------------------------
-- Generalizing (&&&) and (|||)
------------------------------------------------------------------------------
-- | n-ary conjunction
-- If the input list is empty, return a goal that always fails, i.e., returns SNull.
conj :: [State -> SusStream State] -> (State -> SusStream State)
conj goals = undefined

-- | n-ary disjunction
-- If the input list is empty, return a goal that always fails, i.e., returns SNull.
disj :: [State -> SusStream State] -> (State -> SusStream State)
disj goals = undefined


------------------------------------------------------------------------------
-- Stream helper functions
------------------------------------------------------------------------------

-- | Helper function to convert from a SusStream to Haskell list.
-- This is so that we can query concrete results.
-- Note that even if this function is given a `SSuspended stream`,
-- it should still return all of the contents of that stream.
streamToList :: SusStream a -> [a]
streamToList stream = undefined

--------------------------------------------------------------------------------
-- Entry Point
------------------------------------------------------------------------------
-- Entry point to query elements from a SusStream created by applying an
-- empty state to a goal. This is how we will `run` logic programs.
run :: (State -> SusStream State) -> [State]
run g = streamToList (g emptyState)

emptyState :: State
emptyState = (Map.empty, 0)

-- | Return bindings for each given LVar in the given State.
-- (This represents an answer to a logic program query.)
-- Note that if a logic variable is *unbound* in the State's substitution mapping,
-- then it simply appears as itself in the returned list.
answer :: [LVar] -> State -> [Term]
answer lvars (sub, count) = undefined


--------------------------------------------------------------------------------
-- Sample Tests
------------------------------------------------------------------------------

-- | Tests for streamToList (combined using the `and` function).
-- You can uncomment each one individually to help identify problems.
prop_testStreamToList :: Bool
prop_testStreamToList =
  let stream1 = SNull
      stream2 = SCons emptyState stream1
      stream3 = SSuspended stream2
      stream4 = SCons emptyState stream3
      stream5 = SCons emptyState stream2
      stream6 = SSuspended stream1
      stream7 = SSuspended stream5
  in and [
    --    0 == (length $ streamToList stream1)
    --  , 1 == (length $ streamToList stream2)
    --  , 1 == (length $ streamToList stream3)
    --  , 2 == (length $ streamToList stream4)
    --  , 2 == (length $ streamToList stream5)
    --  , 0 == (length $ streamToList stream6)
    --  , 2 == (length $ streamToList stream7)
      ]

-- | Tests for answer (combined using the `and` function).
-- You can uncomment each one individually to help identify problems.
prop_testAnswer :: Bool
prop_testAnswer =
    let v0 = LVar 0
        v1 = LVar 1
        true = TBool True
        false = TBool False
        state = (Map.fromList [(v0, true), (v1, TPair false (TVar v0))], 2)
    in and [
    --   [TVar v0] == answer [v0] emptyState
    -- , [true] == answer [v0] state
    -- , [true, TPair false true] == answer [v0, v1] state
    ]

-- | Test the logic program `x === TInt 1`.
prop_testEqualitySimple :: Bool
prop_testEqualitySimple =
  let answers = map (answer [LVar 0]) -- The logic variable x is LVar 0
                    (run (fresh 1 (\[x] -> x === TInt 1)))
  in and [
    -- we should only get 1 answer
    length answers == 1,
    -- the answer is to set the 0th logic variable to (TInt 1)
    answers == [[TInt 1]]]

-- | Test the logic program `x === term`, where `term` is arbitrary.
prop_testEqualitySingleTerm :: Term -> Bool
prop_testEqualitySingleTerm term =
  let answers = map (answer [LVar 0])
                    (run (fresh 1 (\[x] -> x === term)))
  in and [length answers == 1,
          answers == [[term]]]

-- | Test the logic program `TPair (TInt 1) (TInt 2) === TPair x x`,
-- which is impossible to satisfy.
prop_testEqualityImpossible :: Bool
prop_testEqualityImpossible =
  let answers = map (answer [LVar 0])
                    (run (fresh 1 (\[x] ->
                      TPair (TInt 1) (TInt 2) === TPair x x)))
  in null answers

-- | Generalization of the previous test.
prop_testEqualityImpossibleTerm :: Term -> Term -> Bool
prop_testEqualityImpossibleTerm t1 t2 =
  let answers = map (answer [LVar 0])
                    (run (fresh 1 (\[x] ->
                      TPair t1 t2 === TPair x x)))
  in t1 == t2 || null answers

-- | Test the logic program `TPair (TInt 1) x === TPair (TInt 1) x`,
-- which is satisfied for any setting of `x`. The resulting answer
-- should be that `x` is unbound.
prop_testEqualityUnbound :: Bool
prop_testEqualityUnbound =
  let answers = map (answer [LVar 0])
                    (run (fresh 1 (\[x] ->
                      TPair (TInt 1) x === TPair (TInt 1) x)))
  in and [length answers == 1,           -- we should get one answer
          answers == [[TVar (LVar 0)]]]  -- where `x` is unbound

-- | Test the logic program `TPair t1 x === TPair y t2` (for arbitrary t1 and t2)
-- and check the bindings for both x and y.
prop_testEqualityTwoLVars:: Term -> Term -> Bool
prop_testEqualityTwoLVars t1 t2 =
  let answers = map (answer [LVar 0, LVar 1])
                    (run (fresh 2 (\[x, y] ->
                      TPair t1 x === TPair y t2)))
  in and [length answers == 1,
          answers == [[t2, t1]]]

-- | Test the logic program `TPair t1 x === x`,
-- which should not result in an answer (this is a cyclic binding).
prop_testEqualityCyclic :: Bool
prop_testEqualityCyclic =
    let result = run (fresh 1 (\[x] -> TPair (TInt 1) x === x))
    in null result -- should not get an answer

--------------------------------------------------------------------------------

-- | Tests for n-ary conjunctions that do have a solution.
prop_testConjTrue:: Bool
prop_testConjTrue =
    let result1 = run (fresh 1 (\[x] -> conj [x === TInt 1,
                                              x === TInt 1]))
        result2 = run (fresh 2 (\[x, y] -> conj [x === TInt 1,
                                                 y === TInt 2]))
        result3 = run (fresh 2 (\[x, y] -> conj [x === TInt 1,
                                                 (TPair x x) === y]))
    in and [length result1 == 1,
            length result2 == 1,
            length result3 == 1]

-- | Tests for n-ary conjunctions that do not have a solution.
prop_testConjFalse :: Bool
prop_testConjFalse =
    let result1 = run (fresh 1 (\[x] -> conj [x === TInt 1,
                                              x === TInt 2]))
        result2 = run (fresh 2 (\[x, y] -> conj [x === TInt 1,
                                                 y === TInt 2,
                                                 x === y]))
        result3 = run (fresh 2 (\[x, y] -> conj [x === TInt 1,
                                                 y === TPair x x,
                                                 x === y]))
    in and [null result1,
            null result2,
            null result3]

-- | Test for an n-ary disjunction.
prop_testDisj :: Bool
prop_testDisj =
  let answers = map (answer [LVar 0])
                    (run (fresh 1 (\[x] ->
                      disj [x === TInt 1,
                            x === TInt 0,
                            x === TInt 2])))
  in and [length answers == 3, -- one answer for each choice
          answers == [[TInt 1], [TInt 0], [TInt 2]]]

-- | Test for an n-ary disjunction that contains a duplicate clause.
-- Note that this causes two identical answers to be reported.
prop_testDisjDuplicate :: Bool
prop_testDisjDuplicate =
  let answers = map (answer [LVar 0])
                    (run (fresh 1 (\[x] ->
                      disj [x === TInt 1, x === TInt 1])))
  in and [length answers == 2,
          answers == [[TInt 1], [TInt 1]]]

-- | Test that illustrates using relations to solve a satisfiability
-- problem. Note that the underlying example is from the lecture notes.
prop_testSAT :: Bool
prop_testSAT =
  let true = TBool True
      false = TBool False
      constraint = \[x1, x2, x3, x4] ->
          conj [disj [x1 === true,
                      x2 === false,
                      x4 === true],
                disj [x2 === true,
                      x3 === true,
                      x4 === true],
                disj [x2 === false,
                      x3 === false,
                      x4 === false],
                disj [x1 === false,
                      x3 === true,
                      x4 === false],
                disj [x1 === true,
                      x2 === true,
                      x3 === true],
                disj [x1 === true,
                      x2 === true,
                      x4 === false]]
      answers = map (answer [LVar 0, LVar 1, LVar 2, LVar 3])
                    (run (fresh 4 constraint))
  in not (null answers)  -- we will get duplicate results


-------------------------------------------------------------------------------
-- Main (runs sample tests)
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- You can remove this line as you uncomment the tests below.
    return ()
    -- helper functions: streamToList, answer
    -- quickCheck prop_testStreamToList
    -- quickCheck prop_testAnswer

    -- equality tests
    -- quickCheck prop_testEqualitySimple
    -- quickCheck prop_testEqualitySingleTerm
    -- quickCheck prop_testEqualityImpossible
    -- quickCheck prop_testEqualityImpossibleTerm
    -- quickCheck prop_testEqualityUnbound
    -- quickCheck prop_testEqualityTwoLVars
    -- quickCheck prop_testEqualityCyclic

    -- conj/disj
    -- quickCheck prop_testConjTrue
    -- quickCheck prop_testConjFalse
    -- quickCheck prop_testDisj
    -- quickCheck prop_testDisjDuplicate

    -- satisfiability
    -- quickCheck prop_testSAT
