import Data.Foldable1 (Foldable1(foldrMap1))

newtype Endom a = Endom { appEndom :: a -> a }

instance Semigroup (Endom a) where
    Endom f <> Endom g = Endom (f . g)

instance Monoid (Endom a) where
    mempty = Endom id

newtype SomeMaybe a = SomeMaybe { value :: Maybe a }

instance Semigroup a => Semigroup (SomeMaybe a) where
    SomeMaybe a <> SomeMaybe b
        | Nothing <- a = SomeMaybe b
        | Nothing <- b = SomeMaybe a
        | Just x1 <- a, Just x2 <- b = SomeMaybe (Just (x1 <> x2))

instance Monoid a => Monoid (SomeMaybe a) where
    mempty = SomeMaybe Nothing


or' :: [Bool] -> Bool
or' = foldr (||) False

length' :: [a] -> Int
length' = foldl (\n _ -> n + 1) 0

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max

head' :: [a] -> a
head' = foldl1 const

last' :: [a] -> a
last' = foldl1 (\_ b -> b) 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x r -> if f x then x:r else r) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x r -> f x : r) []


take' :: Int -> [a] -> [a]
take' n xs = foldr step ini xs n
    where
        step :: a -> (Int -> [a]) -> Int -> [a]
        step x g n
            | n > 0 = x : g (n - 1) -- (1)
            | otherwise = [] -- (2)
        ini :: Int -> [a]
        ini = const []

drop' :: Int -> [a] -> [a]
drop' n xs = foldr step ini xs n
    
step :: a -> (Int -> [a]) -> Int -> [a]
step x g n
  | n > 0 = g (n - 1)
  | otherwise = x : g (n - 1)

ini :: Int -> [a]
ini = const []
