{- CSC324 Fall 2018: Assignment 2, Part 3

*Before starting, please review the exercise guidelines at
URL *
-}
module A2Interpreter (
    tcar, tcdr, tapp, tlam,
    questionsToTheAnswer, secondFunction) where

-- Import data definitions and helper functions from the exercise code.
import Ex9 (Term(..), LVar(..), Substitution, unify, walkDeep)
-- Import core microKanren functions from Part 1.
import A2Core (State, emptyState,
               SusStream(SNull, SCons, SSuspended),
               (===), (&&&), (|||), fresh, conj, disj,
               answer, run)
-- Import useful list functions and relations from Part 2.
import A2Reln (makeList, makeAssoc, lookupAssoc, lookupo)

import qualified Data.Map.Strict as Map
import Test.QuickCheck (quickCheck)

------------------------------------------------------------------------------
-- Interpreter Helper Functions
------------------------------------------------------------------------------

t' :: Term -> Term
t' x = TPair (TSymbol "quote") x

tvar :: Term -> Term
tvar name = TPair (TSymbol "var") name

tcons :: Term -> Term -> Term
tcons x xs = TPair (TSymbol "cons") (TPair x xs)

-- complete these:
tcar :: Term -> Term
tcar (TPair (TSymbol "cons") (TPair x xs)) = TPair (TSymbol "car") x
tcar term = TPair (TSymbol "car") term

tcdr :: Term -> Term
tcdr (TPair (TSymbol "cons") (TPair x xs)) = TPair (TSymbol "cdr") xs
tcdr term = TPair (TSymbol "cdr") term

tapp :: Term -> Term -> Term
--tapp proc (TVar (LVar x)) = (TPair (TSymbol "app") (TPair proc (t' (TVar (LVar x))))) 
tapp proc arg = (TPair (TSymbol "app") (TPair proc arg)) 

tlam :: Term -> Term -> Term
--tlam var (TVar (LVar x)) = TPair (TSymbol "lam") (TPair var (t' (TVar (LVar x))))
tlam var body = TPair (TSymbol "lam") (TPair var body)

-- We will need a representation for a closure, even though it is not
-- technically a part of the language grammar.

tclo :: Term -> Term -> Term -> Term
--tclo env var (TVar (LVar x)) = TPair (TSymbol "clo") (TPair env (TPair var (t' (TVar (LVar x)))))
tclo env var body = TPair (TSymbol "clo") (TPair env (TPair var body))

------------------------------------------------------------------------------
-- The interpreter `eval` for a small language consisting of
--    * `cons`, `car`, `cdr`
--    * function definition
--    * function application
------------------------------------------------------------------------------

eval :: Term -> Term -> Term
-- literal:
eval env (TPair (TSymbol "quote") term) = term
-- var:
eval env (TPair (TSymbol "var") var) = lookupAssoc env var
-- builtins
eval env (TPair (TSymbol "cons") (TPair x y)) =
    TPair (eval env x) (eval env y)
eval env (TPair (TSymbol "car") x) =
    case eval env x of
        TPair a _ -> a
        _         -> error "car expects a pair"
eval env (TPair (TSymbol "cdr") x) =
    case eval env x of
        TPair _ b -> b
        _         -> error "cdr expects a pair"
-- lambda
eval env (TPair (TSymbol "lam") (TPair var body)) = tclo env var body
-- apply
eval env (TPair (TSymbol "app") (TPair proc arg)) =
    let p = eval env proc
        a = eval env arg
    in case p of
        TPair (TSymbol "clo") (TPair env2 (TPair var body)) ->
            eval (TPair (TPair var a) env2) body
        _ -> error "app expects a closure"
eval _ _ = error "error"

-- | Sample tests for eval, to illustrate the language.
prop_eval_cons :: Bool
prop_eval_cons = TPair (TInt 1) (TInt 0) ==
   eval TNull (tcons (t' (TInt 1)) (t' (TInt 0)))

prop_eval_app :: Bool
prop_eval_app = (TInt 2) ==
   eval TNull (tapp (tlam (TSymbol "x") (tcar (tvar (TSymbol "x"))))
                    (t' (makeList [(TInt 2), (TInt 3)])))

prop_eval_app2 :: Bool
prop_eval_app2 = (TInt 3) ==
   eval TNull (tapp (tlam (TSymbol "x") (tcar (tcdr (tvar (TSymbol "x")))))
                    (t' (makeList [(TInt 2), (TInt 3)])))


------------------------------------------------------------------------------
-- The relational interpreter `evalo`
------------------------------------------------------------------------------
evalo :: Term -> Term -> Term -> (State -> SusStream State)
evalo env expr output = disj [
    -- quote
    expr === (t' output),
    -- builtins: car
    fresh 2 (\[lst, rest] ->
        conj [expr === tcar lst,
              evalo env lst (TPair output rest)]),
    -- builtins: cdr
    fresh 2 (\[lst, first] ->
        conj [expr === tcdr lst,
              evalo env lst (TPair first output)]),
    -- variable
    fresh 1 (\[var] ->
        conj [expr === tvar var,
              lookupo env var output]),
    -- builtins: cons
    fresh 4 (\[x, y, x2, y2] ->
        conj [expr === (TPair (TSymbol "cons") (TPair x y)),
              output === (TPair x2 y2),
              evalo env x x2,
              evalo env y y2]),
    -- lambda
    fresh 2 (\[var, body] ->
        conj [expr === tlam var body,
              output === tclo env var body]),
    -- function application
    fresh 6 (\[proc, body, env2, arg, var, a] ->
        conj [expr === tapp proc arg,
              evalo env proc (tclo env2 var body),
              evalo env arg a,
              evalo (TPair (TPair var a) env2) body output])
    ]

-- | A version of `eval` using `evalo`, for testing purposes.
evalUsingEvalo :: Term -> Term -> Term
evalUsingEvalo env expr =
    let stream = run (fresh 1 (\[output] -> evalo env expr output))
    in head (answer [LVar 0] (head stream))

compareEvalEvalo :: Term -> Bool
compareEvalEvalo expr =
    let evalResult = eval TNull expr
        evaloResult = evalUsingEvalo TNull expr
    in evalResult == evaloResult

exampleList :: Term
exampleList = makeList [TInt 1, TInt 2]
exampleList2 :: Term
exampleList2 = makeList [TInt 3, TInt 4, TInt 5]

-- | Sample tests for evalo. (appendo must be implemented correctly
-- for these tests to pass).
prop_evalo_cons :: Bool
prop_evalo_cons = compareEvalEvalo (tcons (t' (TInt 1)) (t' (TInt 0)))

prop_evalo_cdr :: Bool
prop_evalo_cdr = compareEvalEvalo (tcdr (t' exampleList))

prop_evalo_app :: Bool
prop_evalo_app = compareEvalEvalo
    (tapp (tlam (TSymbol "x") (tcdr (tvar (TSymbol "x"))))
          (t' exampleList2))


------------------------------------------------------------------------------
-- The Question to Life
------------------------------------------------------------------------------

-- | A list of 42 expressions in our relational language that evaluate to TInt 42.
questionsToTheAnswer :: [Term]
questionsToTheAnswer = 
    take 42 (map head $ map (answer [LVar 0]) (run $ fresh 2 $ \[body, input] ->
        evalo TNull
              (tapp (tlam (TSymbol "x") body) input)
              (TInt 42)))


------------------------------------------------------------------------------
-- Program Synthesis: a constant function
------------------------------------------------------------------------------
-- | Example showing how to synthesize the body of a constant function
-- that always returns 2. Note that one of the fresh logic variables
-- represents the function *body*.
constantFunctionsQuery :: [State]
constantFunctionsQuery = run $ fresh 2 $ \[body, input] ->
    evalo TNull
          (tapp (tlam (TSymbol "x") body) input)
          (TInt 2)

-- TPair (TSymbol "app") 
-- (TPair (TPair (TSymbol "lam") (TPair (TSymbol "x") (TVar (LVar 0)))) (TVar (LVar 1)))

-- | Synthesized constant functions that always return (TInt 2).
constantFunctions :: [Term]
constantFunctions =
    -- LVar 0 is the body of the function
    map head $ map (answer [LVar 0]) constantFunctionsQuery

-- | Sample test illustrating the first solution found by constantFunctions:
-- the body of the function is simply `TInt 2`.
prop_constantFunction :: Bool
prop_constantFunction = head constantFunctions ==
    (TInt 2)

-- (fromList [(LVar 2,TPair (TSymbol "lam") (TPair (TSymbol "x") (TVar (LVar 0)))),
--         (LVar 3,TPair (TSymbol "quote") (TInt 2)),
--         (LVar 4,TNull),
--         (LVar 5,TPair (TSymbol "quote") (TVar (LVar 7))),
--         (LVar 6,TSymbol "x"),
--         (LVar 8,TSymbol "x")],10)


------------------------------------------------------------------------------
-- Program Synthesis: the function `second`
------------------------------------------------------------------------------
-- | Run a query similar to constantFunctionsQuery to synthesize the body
-- of a function that takes a list and returns the second element in the list.
-- You'll need to extract just a single "answer" from the query to store in
-- `secondFunction`.
secondFunction :: Term
secondFunction = undefined


-------------------------------------------------------------------------------
-- Main (runs sample tests)
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- eval
    quickCheck prop_eval_cons
    quickCheck prop_eval_app
    quickCheck prop_eval_app2

    -- evalo
    quickCheck prop_evalo_cons
    quickCheck prop_evalo_cdr
    quickCheck prop_evalo_app

    -- Program Synthesis: a constant function
    quickCheck prop_constantFunction
