import Data.Function

plus1 :: Int -> Int
plus1 x = x + 1

nats :: [Int]
nats = 1: map plus1 nats

minus1 :: Int -> Int
minus1 x = x - 1

negs :: [Int]
negs = -1 : map minus1 negs

-- a swing Integer list 
allInt :: [Int]
allInt = (concat $ zipWith (\x y -> [x,y]) nats negs)

-- rats :: [Rational]
-- rats = concatMap (\x -> (map (\y -> (x/y)) allInt)) allInt

-- rats :: [double]
-- rats = take 10 (iterate (\x -> iterate (\y -> x + y) 1) 0)

-- should be like create two lists, numerator and denomenator, then I can do zipwith

-- generate :: [double] -> [double]
-- allrats = 1 : 1 : map

--pascal's triangle:

--step1: represent each line
pascal :: Int -> [Int]
pascal 0 = [1]
pascal 1 = [1,1] -- == [0,1] + [1,0]
pascal 2 = [1,2,1]  -- == [0,1,1] + [1,1,0]
pascal n = zipWith (+) ([0] ++ pascal (n-1)) (pascal (n-1) ++ [0])
--step2: represent the infinite list

pascalList = [1] : map (\xs -> zipWith (+) ([0] ++ xs) (xs ++ [0])) pascalList


--back to rational:
rats :: Int -> [[Int]]
rats 0 = [[1,1]]
rats 1 = [[1,2],[2,1]] -- [1,2] == [0,1] + [1,1], [2,1] = [1,1] + [1,0]
rats 2 = [[1,3],[3,2],[2,3],[3,1]] 

rats n = concatMap (\xs -> 
    do
        let
            a = head xs
            b = head (tail xs)
            in 
              [[a,a+b],[a+b,b]]) (rats (n-1))

ratss = [[1,1]] : map (\xs -> (concatMap (\ys ->
    do
        let
            a = head ys
            b = head (tail ys)
            in 
              [[a,a+b],[a+b,b]]) xs)
              ) ratss 
posratssreal = concatMap (\xs -> map (\ys ->
    do
        let
            nu = head ys
            de = head (tail ys)
            in
                (fromIntegral nu) / (fromIntegral de)) xs
                ) ratss
negratssreal = map (\x -> (0 - x)) posratssreal

-- FINALLLYYYYYYYYYYYY!!!!!!!!!!!! YAYYYYYYYYYYYYY
rassreal = 0: (concat $ zipWith (\x y -> [x,y]) negratssreal posratssreal)

-- so each row, the true value is 

ratsfloat :: Int -> [Float]
ratsfloat n = map (\xs ->
    do
        let
            nu = head xs
            de = head (tail xs)
            in
                (fromIntegral nu) / (fromIntegral de)) (rats n)

-- then it's like what we do in exercise, combine all of them

f1 x y z = (head z) (x == y)

f2 x y = 
    if x
        then y x
        else
            Just

f3 r s t = do
    x <- r
    y <- s
    return (t x y)