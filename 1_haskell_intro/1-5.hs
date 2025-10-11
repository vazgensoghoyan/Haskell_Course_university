-- 1
doubleFact :: Integer -> Integer
doubleFact n
    | n < 0 = error "n must be non negative"
    | n <= 1 = 1
    | otherwise = n * doubleFact(n - 2)

-- 2 
helper a b c n
        | n < 0 = error "n must be non negative"
        | n == 0 = a
        | otherwise = helper b c (c - 2*b + 3*a) (n-1) 

seqB :: Integer -> Integer
seqB = helper 1 2 3

-- 3
fibonacci :: Integer -> Integer
fibonacci = helper 0 1
    where 
        helper a b n
            | n == 0   = a
            | n > 0    = helper b (a+b) (n-1)
            | n < 0    = helper (b-a) a (n+1)

-- 4
addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x 
    | x < 0 = sum'n'count (-x)
    | x < 10 = (x, 1)
    | otherwise = addPairs (x `rem` 10, 1) (sum'n'count (x `div` 10))

-- 5
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper f a b 1 1000000 
    where
        helper f a b i cnt
            | i == cnt = 0
            | otherwise = (f st0 + f st1) * (st1-st0) / 2 + helper f a b (i+1) cnt
                where {
                    step = (b - a) / cnt; 
                    st0 = a + step * (i-1); 
                    st1 = a + step * i;
                }
