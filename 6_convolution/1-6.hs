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
reverse' = foldr fun' ini'
fun' :: a -> [a] -> [a]
fun' a arr = arr ++ [a]
ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' :: [a] -> a -> [a]
fun'' arr a = a:arr
ini'' = []
