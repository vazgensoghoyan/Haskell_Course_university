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
tails' = foldr fun ini
fun :: a -> [[a]] -> [[a]]
fun x arr = (x : head arr) : arr
ini :: [[a]]
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' :: a -> [[a]] -> [[a]]
fun' x arr = [] : map (x:) arr
ini' :: [[a]]
ini' = [[]]
