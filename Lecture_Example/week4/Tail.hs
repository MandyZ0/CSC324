{-# LANGUAGE BangPatterns #-}

count :: Int -> Int
count n =
    if n == 0
    then
        0
    else
        1 + count (n - 1)


-- Try this with and without the exclamation mark in front of acc.
countTail :: Int -> Int -> Int
countTail n !acc =
    if n == 0
    then
        acc
    else
        countTail (n - 1) (acc + 1)


main :: IO ()
main = print (countTail 15000000 0)
