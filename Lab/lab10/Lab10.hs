{- CSC324 Fall 2018 Lab 10 -}

module Lab10 where


-- |
-- = Task 1
mapMaybes :: (a -> b) -> [Maybe a] -> [Maybe b]
mapMaybes = undefined

composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe = undefined

foldMaybe :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybe = undefined

applyBinaryMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
applyBinaryMaybe = undefined

collectMaybes :: [Maybe a] -> Maybe [a]
collectMaybes = undefined


-- |
-- = Task 2
mapF :: Functor f => (a -> b) -> [f a] -> [f b]
mapF = undefined

composeM :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
composeM = undefined

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM = undefined

applyBinaryM :: Monad m => (a -> b -> c) -> m a -> m b -> m c
applyBinaryM = undefined

collectM :: Monad m => [m a] -> m [a]
collectM = undefined
