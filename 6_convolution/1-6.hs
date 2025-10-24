import Data.List (unfoldr)
import Data.Char (ord, chr)

-- task 1
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun1

fun1 :: (Char, Char) -> Maybe (Char, (Char, Char))
fun1 (a, b) 
    | ord a > ord b = Nothing
    | otherwise = Just (b, ( a, chr (ord b - 1) ))

-- task 2
tails' :: [a] -> [[a]]
tails' = foldr fun2 ini2
fun2 :: a -> [[a]] -> [[a]]
fun2 x arr = (x : head arr) : arr
ini2 :: [[a]]
ini2 = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun2' ini2'
fun2' :: a -> [[a]] -> [[a]]
fun2' x arr = [] : map (x:) arr
ini2' :: [[a]]
ini2' = [[]]

-- task 3
reverse' :: [a] -> [a]
reverse' = foldr fun3' ini3'
fun3' :: a -> [a] -> [a]
fun3' a arr = arr ++ [a]
ini3' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun3'' ini3''
fun3'' :: [a] -> a -> [a]
fun3'' arr a = a:arr
ini3'' = []

-- task 4
infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun4 ini4 xs n 

fun4 :: a -> (Int -> Maybe a) -> Int -> Maybe a
fun4 head tail n
    | n < 0 = Nothing
    | n == 0 = Just head
    | otherwise = tail (n-1)

ini4 :: Int -> Maybe a
ini4 = const Nothing

-- task 5
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v

fun :: (b -> a -> b) -> a -> (b -> t4) -> b -> t4
fun f head tail v = tail (v `f` head)

ini :: a -> a
ini = id
