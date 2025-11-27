{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad (replicateM)
import Control.Monad.Except
import Data.IORef
import System.Random

-- task 1

data Logged a = Logged String a deriving (Eq, Show)

instance Functor Logged where
  fmap f (Logged s a) = Logged s (f a)

instance Applicative Logged where
  pure x = Logged "" x
  (Logged s f) <*> (Logged s' x) = Logged (s' ++ s) (f x)

instance Monad Logged where
  return = pure
  (Logged s a) >>= f =
    let Logged s' b = f a
    in Logged (s' ++ s) b

write2log :: String -> Logged ()
write2log msg = Logged msg ()

-- task 2

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR b xs = writer (go b xs)
  where
    go :: (Show a, Num a) => a -> [a] -> (a, String)
    go b []     = (b, show b)
    go b (x:xs) =
      let (r, lr) = go b xs 
          val     = x - r
          logStr  = "(" ++ show x ++ "-" ++ lr ++ ")"
      in (val, logStr)

-- task 3

minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL acc [] = do
    tell (show acc)
    return acc
minusLoggedL acc xs = do
    let (result, log) = foldl step (acc, "") xs
    tell log
    return result
  where
    step (accum, log) x =
        let newAcc = accum - x
            newLog = if null log
                     then "(" ++ show accum ++ "-" ++ show x ++ ")"
                     else "(" ++ log ++ "-" ++ show x ++ ")"
        in (newAcc, newLog)

-- task 4

fib :: Int -> Integer
fib n = fst $ execState (go n) (0,1)
  where
    go 0 = return ()
    go k = do
        fibStep
        go (k-1)

fibStep :: State (Integer,Integer) ()
fibStep = do
  (a,b) <- get
  put (b, a+b)

-- task 5

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
  val <- readIORef ref
  if p val
    then do
      action
      while ref p action
    else return ()

-- task 6


avgdev :: Int -> Int -> IO Double
avgdev k n = do
    deviations <- replicateM k $ do
        series <- replicateM n $ randomRIO (0 :: Int, 1)
        let heads = sum series
            dev   = abs (fromIntegral heads - fromIntegral n / 2)
        return dev
    return $ sum deviations / fromIntegral k

-- task 7

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState bounds = do
    gen <- get
    let (val, gen') = randomR bounds gen
    put gen'
    return val

flipSeries :: Int -> State StdGen Int
flipSeries n = do
    flips <- replicateM n (randomRState (0 :: Int, 1))  -- 0=решка, 1=орёл
    return $ sum flips

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
    deviations <- replicateM k $ do
        heads <- flipSeries n
        return $ abs (fromIntegral heads - fromIntegral n / 2)
    return $ sum deviations / fromIntegral k

-- task 8

avgdev'' :: Int -> Int -> Double
avgdev'' k n =
    let gen = mkStdGen 777
        flips = randomRs (0 :: Int, 1) gen
        series = take (k * n) flips
        chunks [] = []
        chunks xs = take n xs : chunks (drop n xs)
        deviations = map (\s -> abs (fromIntegral (sum s) - fromIntegral n / 2)) (chunks series)
    in sum deviations / fromIntegral k

-- task 9

data ListIndexError = 
    ErrTooLargeIndex Int 
  | ErrNegativeIndex 
  | OtherErr String
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: MonadError ListIndexError m => [a] -> Int -> m a
xs !!! n
    | n < 0     = throwError ErrNegativeIndex
    | otherwise = go xs n n
  where
    go [] _ origIdx   = throwError (ErrTooLargeIndex origIdx)
    go (y:ys) 0 _     = return y
    go (_:ys) i origIdx = go ys (i-1) origIdx
