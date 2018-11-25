{- CSC324 Fall 2018 Lab 10 -}

module Lab10 where
import Debug.Trace


-- |
-- = Task 1

mapMaybes :: (a -> b) -> [Maybe a] -> [Maybe b]
mapMaybes f [] = []
mapMaybes f (a:as) = 
    do
        case a of
            Nothing -> [Nothing] ++ mapMaybes f as
            Just atype -> [Just (f atype)] ++ mapMaybes f as


composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe f g = 
    (\x ->
        do 
            case f x of
                Nothing -> Nothing
                Just btypevar -> 
                    case g btypevar of
                        Nothing -> Nothing
                        Just ctypevar -> Just ctypevar
    )

foldMaybe :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybe f b [] = Just b
foldMaybe f b (a:as) = 
    do 
        case f b a of
            Nothing -> Nothing
            Just btypevar -> foldMaybe f btypevar as

applyBinaryMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
applyBinaryMaybe f Nothing b = Nothing
applyBinaryMaybe f (Just a) Nothing = Nothing
applyBinaryMaybe f (Just a) (Just b) = Just (f a b) 

collectMaybes :: [Maybe a] -> Maybe [a]
collectMaybes [] = Just []
collectMaybes (Nothing:as) = Nothing
collectMaybes ((Just a):as) = 
    do 
        case collectMaybes as of
            Nothing -> Nothing
            Just alst -> Just ([a] ++ alst)



-- |
-- = Task 2
mapF :: Functor f => (a -> b) -> [f a] -> [f b]
mapF g [] = []
mapF g (a:as) = [fmap g a] ++ mapF g as

composeM :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
composeM f g = 
    (\a -> f a >>= g)

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM f b [] = return b
foldM f b (a:as) = (>>=) (f b a) (\x -> (foldM f x as))

applyBinaryM :: Monad m => (a -> b -> c) -> m a -> m b -> m c
applyBinaryM f a b = 
   b >>= (\y -> a >>= (\x -> return (f x y)))

collectM :: Monad m => [m a] -> m [a]
collectM [] = return []
collectM (a:as) = 
    (a >>=
        \y -> collectM as >>=
            \ys ->return (y:ys)