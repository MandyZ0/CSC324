import Text.Read (readMaybe)


-- | Read an integer.
readInt :: String -> Maybe Int
readInt s = readMaybe s

-- | Safe list indexing.
safeIndex :: [a] -> Int -> Maybe a
safeIndex lst i =
    if i >= 0 && i < length lst
    then
        Just (lst !! i)
    else
        Nothing

-- | Take a list and two strings, and add the elements
-- at the corresponding indexes.
processDataUnsafe :: [Int] -> String -> String -> Int
processDataUnsafe items s1 s2 =
    let index1 = read s1
        index2 = read s2
        item1 = items !! index1
        item2 = items !! index2
    in
        item1 + item2

-- | Take a list and two strings, and add the elements
-- at the corresponding indexes.
processData :: [Int] -> String -> String -> Maybe Int
processData items s1 s2 =
    case readInt s1 of
        Nothing -> Nothing
        Just index1 ->
            case readInt s2 of
                Nothing -> Nothing
                Just index2 ->
                    case safeIndex items index1 of
                        Nothing -> Nothing
                        Just item1 ->
                            case safeIndex items index2 of
                                Nothing -> Nothing
                                Just item2 ->
                                    Just (item1 + item2)


-- | Take a list and two strings, and add the elements
-- at the corresponding indexes. ("andThen" version)
processData1 :: [Int] -> String -> String -> Maybe Int
processData1 items s1 s2 =
    readInt s1 `andThen` \index1 -> (
        readInt s2 `andThen` \index2 -> (
            safeIndex items index1 `andThen` \item1 -> (
                safeIndex items index2 `andThen` \item2 -> (
                    Just (item1 + item2)
                )
            )
        )
    )

-- | Take a list and two strings, and add the elements
-- at the corresponding indexes. (Monad version)
-- Note that `Just` has been replaced by `return`
processData2 :: [Int] -> String -> String -> Maybe Int
processData2 items s1 s2 =
    readInt s1 >>= \index1 -> (
        readInt s2 >>= \index2 -> (
            safeIndex items index1 >>= \item1 -> (
                safeIndex items index2 >>= \item2 -> (
                    return (item1 + item2)
                )
            )
        )
    )


-- | Take a list and two strings, and add the elements
-- at the corresponding indexes. (Cleaner syntax version)
processData3 :: [Int] -> String -> String -> Maybe Int
processData3 items s1 s2 =
    readInt s1 >>= \index1 ->
    readInt s2 >>= \index2 ->
    safeIndex items index1 >>= \item1 ->
    safeIndex items index2 >>= \item2 ->
    return (item1 + item2)


-- | Take a list and two strings, and add the elements
-- at the corresponding indexes. (do notation version)
processData4 :: [Int] -> String -> String -> Either String Int
processData4 items s1 s2 = do
    index1 <- readIntEither s1
    index2 <- readIntEither s2
    item1 <- safeIndexEither items index1
    item2 <- safeIndexEither items index2
    return (item1 + item2)


-- | Read an integer. ("Either" version)
readIntEither :: String -> Either String Int
readIntEither s =
    case readMaybe s of
        Nothing -> Left ("Could not parse \"" ++ s ++ "\" as an integer.")
        Just n -> Right n


-- | Safe list indexing.
safeIndexEither :: [a] -> Int -> Either String a
safeIndexEither lst i =
    if i >= 0 && i < length lst
    then
        Right (lst !! i)
    else
        Left ("Index " ++ show i ++ " out of bounds.")


-- Helpers
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing `andThen` _  = Nothing
(Just x) `andThen` f = f x
