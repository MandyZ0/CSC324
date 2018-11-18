{- CSC324 Fall 2018 Lab 9 -}

module Lab9 where

-- |
-- = Task 1: Fitting some generic functions
-- For each of the functions below, give an implementation
-- of them so that they are *total* (always terminate and don't raise
-- and error), and compile with the given type signatures.

-- Feel free to change the function and parameter names to be more descriptive!

-- **** don't know whether it's right or not

-- Note that (_, _) is the Haskell *tuple* type, which is also generically polymorphic.
f0 :: a -> (a, a)
f0 x = (x,x)

f1 :: (a -> b -> c) -> a -> b -> c
f1 f x y = f x y

f2 :: (b -> c) -> (a -> b) -> a -> c
f2 g h x = g (h x)

-- What's special about this one?
f3 :: (a, b) -> (c -> b)
f3 (x, y) = (\c -> y)


-- |
-- = Task 2: One new generic type, `Maybe`

-- For your reference, here's the definition of the `Maybe` type built into Haskell:
-- data Maybe = Nothing | Just a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []= Nothing
safeTail (_:xs) = Just xs

onlyWhen :: (a -> Bool) -> a -> Maybe a
onlyWhen f x = 
    if (f x) then Just x else Nothing
    


try :: (a -> b) -> Maybe a -> Maybe b
try f (Just a) = Just (f a) 
try f _ = Nothing


-- |
-- = Task 3: Introduction to typeclasses

data Shape
    = Circle Float            -- ^ A circle with the given radius
    | Rectangle Float Float   -- ^ A rectangle with the given width and height
    | Square Float            -- ^ A square with the given side length
