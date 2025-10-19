import Data.Complex

-- task 1
newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
    show (Matrix []) = "EMPTY"
    show (Matrix [x]) = show x
    show (Matrix (x:xs)) = show x ++ "\n" ++ show (Matrix xs)

-- task 2
newtype Cmplx = Cmplx (Complex Double) deriving Eq

instance Show Cmplx where
    show (Cmplx (a :+ b)) = show a ++ (if b < 0 then "-i*" else "+i*") ++ show (abs b)

instance Read Cmplx where
    readsPrec _ s = do
        (real, real_rest) <- reads s :: [(Double, String)]
        (rest, sign) <- case real_rest of
            ('+':'i':'*':rest) -> return (rest, 1)
            ('-':'i':'*':rest) -> return (rest, -1)
            _ -> []
        (imag, imag_rest) <- reads rest :: [(Double, String)]
        return (Cmplx (real :+ (sign * imag)), imag_rest)

-- task 3
class (Eq a, Bounded a, Enum a) => SafeEnum a where
    ssucc :: a -> a
    spred :: a -> a

    ssucc x
        | x == maxBound = minBound
        | otherwise     = succ x

    spred x
        | x == minBound = maxBound
        | otherwise     = pred x

-- task 4
rotate :: Int -> [a] -> [a]
rotate n xs
    | null xs = []
    | n >= 0 =
        let (front, back) = splitAt n xs
            in case back of
                [] -> rotate (n `mod` len) xs
                _ -> back ++ front
    | n < 0 = drop start xs ++ take start xs
    where
        len = length xs
        start = ((n `rem` len) + len) `rem` len

-- task 5
comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb k (x:xs) = map (x:) (comb (k-1) xs) ++ comb k xs
