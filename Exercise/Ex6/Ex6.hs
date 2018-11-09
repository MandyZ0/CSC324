{- CSC324 Fall 2018: Exercise 6

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}

-- The module definition line, including exports. Don't change this!
module Ex6 (expressionsOfRank,
            arithmeticExpressions,
            BinaryGrammar(..),
            expressionsOfRankGen,
            expressions) where

-- You *may* add imports from Data.List (but no other imports).
import Data.List (sort)
import Test.QuickCheck (Property, quickCheck)


-------------------------------------------------------------------------------
-- |
-- = Task 1: A small arithmetic expression grammar

-- | Helpers to define the different parts of the grammar.
-- Remember that we're representing all types of expressions as strings here.
numbers :: [String]
numbers = ["1", "2", "3", "4"]
-- ~define makePlus as function
makePlus :: String -> String -> String
makePlus expr1 expr2 = "(+ " ++ expr1 ++ " " ++ expr2 ++ ")"
-- ~define makeTimes as function
makeTimes :: String -> String -> String
makeTimes expr1 expr2 = "(* " ++ expr1 ++ " " ++ expr2 ++ ")"


-- | Returns a list of expressions *of rank k* following the arithmetic expression
-- grammar from the exercise handout. The expressions can be in any order.
-- Order matters in an expression (so `(+ 3 4)` is different from `(+ 4 3)`),
-- and no expression should appear twice in the output list.
--
-- Precondition: k >= 0.
--
-- Hints:
--   1. The only expressions at rank 0 are the numeric literals.
--   2. This function can (and probably should) be recursive!
--   3. Remember that you aren't *evaluating* any expressions here.
--      Don't get distracted trying to "evaluate" "(+ 3 4)" into "7".
--   4. Make helper function(s)! This function is quite elegant if you do some
--      design work to think about what helper(s) you really need here.
--   5. Spend some time reading through the Haskell List documentation
--      to get some ideas about the functions you might find useful.
--      https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html
--      In particular, concatMap is pretty sweet. :)
--
--      Along the same lines, http://learnyouahaskell.com/starting-out#texas-ranges.
--      (Note: [0..(-1)] is an empty list.)
-- ~We define the rank of an expression in this language as the maximum depth of the nesting of its subexpressions. 
-- !! In haskell variable names must start with a lowercase letter. 

sumhelper :: Int -> Int
sumhelper 0 =  4
sumhelper n = sumhelper (n-1) + 2^(6*2^(n-1)-1)

expressionsOfRank :: Int -> [String]
expressionsOfRank 0 = numbers
expressionsOfRank 1 = 
    let
        plusExprk_1_k_1 = concatMap (\ x -> (map (\ y -> (makePlus y x)) numbers)) numbers
        timesExprk_1k_1 = concatMap (\ x -> (map (\ y -> (makeTimes y x)) numbers)) numbers
        in
            plusExprk_1_k_1 ++ timesExprk_1k_1 
expressionsOfRank n = 
            let
                preExprk_1 = expressionsOfRank $! (n-1)
                plusExprk_1_k_1 = concatMap (\ x -> (map (\ y -> (makePlus y x)) preExprk_1)) preExprk_1
                timesExprk_1k_1 = concatMap (\ x -> (map (\ y -> (makeTimes y x)) preExprk_1)) preExprk_1
                allExprk_2 = take (sumhelper (n-2)) arithmeticExpressions
                plusExprk_1k_2 = concatMap (\ x -> (map (\ y -> (makePlus y x)) preExprk_1)) allExprk_2
                timesExprk_1k_2 = concatMap (\ x -> (map (\ y -> (makeTimes y x)) preExprk_1)) allExprk_2
                plusExprk_2k_1 = concatMap (\ x -> (map (\ y -> (makePlus x y)) preExprk_1)) allExprk_2
                timesExprk_2k_1 = concatMap (\ x -> (map (\ y -> (makeTimes x y)) preExprk_1)) allExprk_2
                -- plusExprk_2k_1 = concatMap (\ x -> (map (\ y -> (makePlus y x)) allExprk_2)) preExprk_1
                -- timesExprk_2k_1 = concatMap (\ x -> (map (\ y -> (makeTimes y x)) allExprk_2)) preExprk_1
               
                in
                    plusExprk_1_k_1 ++ timesExprk_1k_1 ++ plusExprk_1k_2 ++  timesExprk_1k_2 ++ plusExprk_2k_1 ++ timesExprk_2k_1
    

-- | An infinite list containing all arithmetic expressions (again, no duplicates),
-- in *non-decreasing rank order*. Expressions of the same rank may appear in any order.
arithmeticExpressions :: [String]
arithmeticExpressions = 
    numbers ++ concat [expressionsOfRank n | n<-[1..]] 



-------------------------------------------------------------------------------
-- | Exact test for rank 0 expressions.
prop_arithRank0Exact :: Bool
prop_arithRank0Exact =
    sort (expressionsOfRank 0) == numbers

-- | Test for number of rank 1 expressions.
prop_arithRank1Length :: Bool
prop_arithRank1Length =
    length (sort (expressionsOfRank 1)) == 2 * (length numbers) * (length numbers)

-- | Most naive implementations will miss this. Be careful here!
-- Also, see the note on the handout about efficiency.
prop_arithRank3ElemCheck :: Bool
prop_arithRank3ElemCheck =
    elem "(+ (* (+ 3 2) 4) 1)" (expressionsOfRank 3)


-------------------------------------------------------------------------------
-- |
-- = Task 2: Generalize!

-- | This data type represents a binary recursive grammar.
-- The first argument to the constructor is a list of the *atoms* of the grammar,
-- and the second is a list of the *recursive rules* of the grammar.
data BinaryGrammar = BinaryGrammar [String] [String -> String -> String]

-- | Example: arithmetic expressions.
arithmeticGrammar :: BinaryGrammar
arithmeticGrammar = BinaryGrammar numbers [makePlus, makeTimes]

-- | Another example: the one from Task 2 in the handout.
lifeChoicesGrammar :: BinaryGrammar
lifeChoicesGrammar =
    BinaryGrammar
    ["cats", "dogs", "bird", "love", "terror", "hunger"]
    [makeAnd, makeOr]

    where
        -- We use the "where" keyword to define some local helper functions.
        -- Note that this is indented inside the definition of lifeChoicesGrammar.
        makeAnd :: String -> String -> String
        makeAnd expr1 expr2 = expr1 ++ " and " ++ expr2 ++ "!"

        makeOr :: String -> String -> String
        makeOr expr1 expr2 = expr1 ++ " or " ++ expr2 ++ "?"


-- | Returns a list of expressions *of rank k* following the given grammar.
-- The expressions can be in any order.
--
-- Precondition: k >= 0.
--
sumhelpergen :: BinaryGrammar -> Int -> Int

sumhelpergen (BinaryGrammar atoms rules) 0 =  length atoms
sumhelpergen (BinaryGrammar atoms rules) n = sumhelper (n-1) + (length rules)^(2^n-1)*(length atoms)^(2^n)

expressionsOfRankGen :: BinaryGrammar -> Int -> [String]
expressionsOfRankGen (BinaryGrammar atoms rules) 0 = atoms
expressionsOfRankGen (BinaryGrammar atoms rules) 1 =
    concatMap (\z -> (concatMap (\ x -> (map (\ y -> (z y x)) atoms)) atoms)) rules

expressionsOfRankGen (BinaryGrammar atoms rules) n =
        let
            grammar = BinaryGrammar atoms rules
            preExprk_1 = expressionsOfRankGen grammar (n-1)
            exprk_1_k_1 = concatMap (\z -> (concatMap (\ x -> (map (\ y -> (z y x)) preExprk_1)) preExprk_1)) rules
            allExprk_2 = take (sumhelpergen grammar (n-2)) (expressions grammar)
            exprk_1k_2 = concatMap (\z -> (concatMap (\ x -> (map (\ y -> (z y x)) preExprk_1)) allExprk_2)) rules
            exprk_2k_1 = concatMap (\z -> (concatMap (\ x -> (map (\ y -> (z y x)) allExprk_2)) preExprk_1)) rules
           
            in
                exprk_1_k_1 ++ exprk_1k_2 ++ exprk_2k_1 


-- | A generalization of arithmeticExpressions.
-- Same restrictions (e.g., ordered by rank) apply.
expressions :: BinaryGrammar -> [String]
expressions (BinaryGrammar atoms rules) = 
    let
        grammar = BinaryGrammar atoms rules
        in
            atoms ++ concat [expressionsOfRankGen grammar n | n<-[1..]] 


-------------------------------------------------------------------------------
-- | A few tests to show that these functions really do generalize Task 1.
prop_expressionsOfRankGen0 :: Bool
prop_expressionsOfRankGen0 =
    sort (expressionsOfRank 0) == sort (expressionsOfRankGen arithmeticGrammar 0)

prop_expressionsOfRankGen1 :: Bool
prop_expressionsOfRankGen1 =
    sort (expressionsOfRank 1) == sort (expressionsOfRankGen arithmeticGrammar 1)


-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_arithRank0Exact
    quickCheck prop_arithRank1Length
    quickCheck prop_arithRank3ElemCheck
    quickCheck prop_expressionsOfRankGen0
    quickCheck prop_expressionsOfRankGen1
