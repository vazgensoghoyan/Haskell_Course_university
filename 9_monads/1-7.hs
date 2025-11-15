{-# LANGUAGE InstanceSigs #-}

-- task 1

surround :: a -> a -> [a] -> [a]
surround x y zs = do
    s <- zs
    [x, s, y]

-- task 2

lookups :: (Eq k) => k -> [(k,v)] -> [v]
lookups x ys = do
    (k, v) <- ys
    [v | k == x]

-- task 3

factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
    d1 <- [1.. floor $ sqrt $ fromInteger n]
    [(d1, n `div` d1) | n `mod` d1 == 0 ]

-- task 4

absDiff :: Num a => [a] -> [a]
absDiff xs = do
    (x, y) <- zip xs (tail xs)
    return $ abs (x - y)

-- task 5

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

oddToList :: OddC a -> [a]
oddToList (Un x) = [x]
oddToList (Bi x y xs) = x : y : oddToList xs

listToOdd :: [a] -> OddC a
listToOdd [x] = Un x
listToOdd (x:y:xs) = Bi x y (listToOdd xs)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC x y z = listToOdd ( oddToList x ++ oddToList y ++ oddToList z )

-- task 6

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi x y xs) = concat3OC x y (concatOC xs)

-- task 7

instance Functor OddC where
    fmap :: (a -> b) -> OddC a -> OddC b
    fmap f (Un x) = Un (f x)
    fmap f (Bi x y xs) = Bi (f x) (f y) (fmap f xs)

instance Applicative OddC where
    (<*>) :: OddC (a -> b) -> OddC a -> OddC b
    f <*> x = concatOC $ fmap (`fmap` x) f

    pure :: a -> OddC a
    pure = Un

instance Monad OddC where
    (>>=) :: OddC a -> (a -> OddC b) -> OddC b
    x >>= f = concatOC $ fmap f x

    return :: a -> OddC a
    return = pure
