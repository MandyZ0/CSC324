{- Illustrating the concept of "higher-order" typeclasses.

class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}
module Mappers where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Sequence


numbersList :: [Int]
numbersList = [1, 2, 3]

numbersSeq :: Sequence.Seq Int
numbersSeq = Sequence.fromList [1, 2, 3]

numbersMap :: Map.Map String Int
numbersMap = Map.fromList [("a", 1), ("b", 2), ("c", 3)]

add10 :: Int -> Int
add10 n = n + 10

add10All :: Functor f => f Int -> f Int
add10All numbers = fmap add10 numbers
