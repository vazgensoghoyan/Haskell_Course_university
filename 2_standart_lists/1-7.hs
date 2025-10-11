--1 
sum2 :: Num a => [a] -> [a] -> [a]
sum2 [] y = y
sum2 x [] = x
sum2 (x:xs) (y:ys) = (x + y) : sum2 xs ys

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 x y z = (x `sum2` y) `sum2` z

-- 2
digits :: Integer -> [Integer]
digits x
    | x < 0 = digits (-x)
    | x < 10 = [x]
    | otherwise = digits (x `div` 10) ++ [x `mod` 10]

-- 3
containsAllDigits :: Integer -> Bool
containsAllDigits n = all (`elem` (digits n)) [1..9]

-- 4
containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes n = all (`elem` filtered) [1..9] && length filtered == 9
  where
    ds = digits n
    filtered = filter (/= 0) ds

-- 5
sublist :: Int -> Int -> [a] -> [a]
sublist a b lst
    | a < 0 = take b lst
    | otherwise = take (b - a) (drop a lst)

--6
repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem _ [] = []
repeatEveryElem n _ | n <= 0 = []
repeatEveryElem n (x:xs) = x *** n ++ repeatEveryElem n xs
    where
        x *** 0 = []
        x *** n = x : x *** (n-1)

--7
movingLists :: Int -> [a] -> [[a]]
movingLists n lst
    | length current < n = []
    | otherwise          = current : movingLists n (tail lst)
  where
    current = take n lst
