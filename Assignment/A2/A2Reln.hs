{- CSC324 Fall 2018: Assignment 2, Part 2

*Before starting, please review the exercise guidelines at
URL *
-}
module A2Reln (
    headoOfLists, memberoOfList, appendoList,
    makeList, makeAssoc, lookupAssoc,
    tailo, lookupo) where

-- Import data definitions and helper functions from the exercise code.
import Ex9 (Term(..), LVar(..), Substitution, unify, walkDeep, extend )
-- Import core microKanren functions from Part 1.
import A2Core (State, emptyState,
               SusStream(SNull, SCons, SSuspended),
               (===), (&&&), (|||), fresh, conj, disj,
               answer, run)

import qualified Data.Map.Strict as Map
import Test.QuickCheck (quickCheck)


------------------------------------------------------------------------------
-- The relation `heado`. The variable `headoOfLists`.
------------------------------------------------------------------------------

-- | The relation `heado`.
heado :: Term -> Term -> (State -> SusStream State)
heado lst item = fresh 1 (\[rest] ->  -- we need a new logic variable `rest`
    (TPair item rest) === lst)      -- unify `lst` with `(TPair item rest)`

-- | The function `t_head`, to compare with the relation `heado`.
t_head :: Term -> Term
t_head lst =
    let TPair item rest = lst -- this "pattern matching" is like `===`
    in item

-- | Run a query to heado with the **list** being a logic variable!
prop_testHeadoList:: Bool
prop_testHeadoList =
    let answers = map (answer [LVar 0])
                      (run (fresh 1 (\[lst] ->
                        heado lst (TInt 0))))
    in head answers == [TPair (TInt 0) (TVar (LVar 1))]

-- | Run a query to heado with the **item** being a logic variable.
prop_testHeadoItem:: Bool
prop_testHeadoItem =
    let answers = map (answer [LVar 0])
                      (run (fresh 1 (\[item] ->
                        heado (TPair (TInt 0) TNull) item)))
    in head answers == [TInt 0]


-- | A query to heado, where *both* arguments are logic variables.
headoOfLists :: State -> SusStream State
headoOfLists (sub,count)= 
    (fresh 2 (\[x,y] -> heado x y) (sub,count))


------------------------------------------------------------------------------
-- The relation `membero`. The variable `memberoOfList`.
------------------------------------------------------------------------------

-- | The relation `membero`.
membero :: Term -> Term -> (State -> SusStream State)
membero item lst = fresh 2 $ \[first, rest] ->
    (TPair first rest) === lst &&&
    (first === item ||| (membero item rest))

-- | The function `t_member`, to compare with the relation `membero`.
-- Note that this raises a runtime error if `item` is not in `lst`.
t_member :: Term -> Term -> Bool
t_member item lst =
    let (TPair first rest) = lst
    in (first == item) || (t_member item rest)

-- | Run a query to membero for the given list `lst`, with a fresh variable
-- for the `item` argument. Running this query should produce a stream
-- containing all possible members of `lst`.
memberoOfList :: Term -> (State -> SusStream State)
memberoOfList lst= fresh 1 $ (\[item]-> membero item lst)

-- | Sample test for memberoOfList for the `testList`.
prop_testMemberoOfListSimple :: Bool
prop_testMemberoOfListSimple =
    let members = map (answer [LVar 0]) (run (memberoOfList testList))
    in
        elem [TInt 1] members  -- TInt 1 is a possible member of testList.

-- | A Term representing the list [1, 2, 3, 4].
testList :: Term
testList = makeList [TInt 1, TInt 2, TInt 3, TInt 4]

-- | Helper function to represent a list of terms.
-- Like always, we define a <List> of terms as chained pairs:
--    <List> = TNull
--           | TPair Term <List>
makeList :: [Term] -> Term
makeList [] = TNull
makeList (x:xs) = TPair x (makeList xs)


------------------------------------------------------------------------------
-- The relation `appendo`. The variable `appendoList`.
------------------------------------------------------------------------------

-- | The relation `appendo`.
appendo :: Term -> Term -> Term -> (State -> SusStream State)
appendo xs ys zs =
  ((xs === TNull) &&& (ys === zs)) |||
  fresh 3 (\[a, b, c] ->
      conj [(TPair a b) === xs,
            (TPair a c) === zs,
            appendo b ys c])

-- | The function `append`, to compare with the relation `appendo`.
append :: Term -> Term -> Term
append xs ys =
    if xs == TNull
    then ys
    else let TPair a b = xs
             c = append b ys
         in TPair a c


-- | Run a query to appendo for the given list `lst` to find all pairs (xs, ys)
-- so that (append xs ys) == lst.
appendoList :: Term -> (State -> SusStream State)
appendoList lst = fresh 2 $ (\[xs,ys]-> appendo xs ys lst)

-- | Sample test for appendoList.
prop_testAppendoResult :: Bool
prop_testAppendoResult =
    let appendoTestList = map (answer [LVar 0, LVar 1]) (run (appendoList testList))
    in and [
        length appendoTestList == 5,
        (appendoTestList !! 0) == [TNull, testList],
        (appendoTestList !! 1) == [makeList [TInt 1],
                                   makeList [TInt 2, TInt 3, TInt 4]],
        (appendoTestList !! 2) == [makeList [TInt 1, TInt 2],
                                   makeList [TInt 3, TInt 4]],
        (appendoTestList !! 3) == [makeList [TInt 1, TInt 2, TInt 3],
                                   makeList [TInt 4]],
        (appendoTestList !! 4) == [testList, TNull]
        ]


------------------------------------------------------------------------------
-- The relation `tailo`.
------------------------------------------------------------------------------

-- | The function `tail` provided to you as a reference.
t_tail :: Term -> Term
t_tail lst =
    let (TPair item rest) = lst
    in rest

-- | Complete the definition `tailo`.
tailo :: Term -> Term -> (State -> SusStream State)
tailo lst rest = fresh 1 (\[first] ->  -- we need a new logic variable `rest`
    (TPair first rest) === lst)

-- | Run a query to tailo with the **item** being a logic variable.
prop_testTailoItem :: Bool
prop_testTailoItem =
    let answers = map (answer [LVar 0])
                      (run (fresh 1 (\[item] ->
                        tailo (TPair (TInt 0) TNull) item)))
    in head answers == [TNull]

-- | Run a query to tailo with the **list** being a logic variable!
prop_testTailoList :: Bool
prop_testTailoList =
    let answers = map (answer [LVar 0])
                      (run (fresh 1 (\[lst] ->
                        tailo lst (TInt 0))))
    in head answers == [TPair (TVar (LVar 1)) (TInt 0)]


------------------------------------------------------------------------------
-- Helper Functions to make associations
------------------------------------------------------------------------------

-- An association list is defined as follows:
--
--     <assoc> = TNull
--             | (TPair (TPair <key> <value>) <assoc>)
--     <key>   = Term
--     <value> = Term

makeAssoc :: [Term] -> Term
makeAssoc [] = TNull
makeAssoc (k:v:kvs) = TPair (TPair k v) (makeAssoc kvs)

testAssoc :: Term
testAssoc = makeAssoc [(TSymbol "x"), (TBool True),
                       (TSymbol "y"), (TBool False)]

testAssocDup :: Term
testAssocDup = makeAssoc [(TSymbol "x"), (TBool True),
                          (TSymbol "x"), (TBool False)]


------------------------------------------------------------------------------
-- The relation `lookupo`
------------------------------------------------------------------------------

-- | Looks up a key in an association list.
-- We assume that the key is in the association list. Otherwise,
-- we will get a Haskell error.
lookupAssoc :: Term -> Term -> Term
lookupAssoc assoc key =
    let (TPair (TPair k v) rest) = assoc
    in if k == key
       then v
       else lookupAssoc rest key

-- | The relation `lookupo` takes three arguments:
-- the association list, key and value. It succeeds if (key, value)
-- is in the association list.
lookupo :: Term -> Term -> Term -> (State -> SusStream State)
lookupo assoc key value =

    fresh 3 $ (\[tkey,tvalue,tassoc] ->
        (TPair (TPair tkey tvalue) tassoc) === assoc &&&
        (((TPair tkey tvalue) === (TPair key value)) ||| (lookupo tassoc key value)))

-- | Sample tests for lookupo.
prop_lookupo1 :: Bool
prop_lookupo1 =
    let result = run (fresh 1 (\[x] -> lookupo testAssoc (TSymbol "y") x))
    in length result == 1

prop_lookupo2 :: Bool
prop_lookupo2 =
    let result = run (fresh 1 (\[x] -> lookupo testAssoc (TSymbol "z") x))
    in length result == 0

prop_lookupo3 :: Bool
prop_lookupo3 =
    let result = run (fresh 1 (\[x] -> lookupo testAssoc x (TBool False)))
    in length result == 1

prop_lookupo4 :: Bool
prop_lookupo4 =
    let result = run (fresh 1 (\[x] -> lookupo testAssocDup (TSymbol "x") x))
    in length result == 2


-------------------------------------------------------------------------------
-- Main (runs sample tests)
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- heado (note: this relation is implemented for you)
    quickCheck prop_testHeadoList
    quickCheck prop_testHeadoItem

    -- memberoOfList
    quickCheck prop_testMemberoOfListSimple

    -- appendoList
    quickCheck prop_testAppendoResult

    -- tailo
    quickCheck prop_testTailoList
    quickCheck prop_testTailoItem

    -- lookupo
    quickCheck prop_lookupo1
    quickCheck prop_lookupo2
    quickCheck prop_lookupo3
    quickCheck prop_lookupo4
