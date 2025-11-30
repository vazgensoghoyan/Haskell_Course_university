{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Writer
    ( MonadIO(liftIO), MonadWriter(tell, writer), Writer )
import Control.Monad.State
    ( MonadState(put, get), execState, State )
import Control.Monad.Except ( ExceptT, MonadError(..) )
import Control.Monad.IO.Class (liftIO)
import Control.Monad 
    (MonadPlus(..), guard, replicateM, when)
import Control.Applicative (Alternative(..))
import Data.Foldable (msum)
import Data.Char
    ( isNumber, isPunctuation, digitToInt, isHexDigit, toUpper )
import Data.IORef
import System.Random

-- task 1

data Logged a = Logged String a deriving (Eq, Show)

instance Functor Logged where
  fmap f (Logged s a) = Logged s (f a)

instance Applicative Logged where
  pure = Logged ""
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
  when (p val) $ do
      action
      while ref p action

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

-- task 10

data Excep a = Err String | Ok a
  deriving (Eq, Show)

instance Functor Excep where
  fmap f (Ok x) = Ok (f x)
  fmap _ (Err e) = Err e

instance Applicative Excep where
  pure = Ok
  Ok f  <*> Ok x  = Ok (f x)
  Err e <*> _ = Err e
  _ <*> Err e = Err e

instance Monad Excep where
  Ok x >>= f = f x
  Err e >>= _ = Err e
  return = pure

instance MonadFail Excep where
  fail _ = Err "Monad.fail error."

instance Alternative Excep where
  empty = Err "Alternative.empty error."
  Ok x <|> _ = Ok x
  Err _ <|> r = r

instance MonadPlus Excep where
  mzero = empty
  mplus = (<|>)

instance MonadError String Excep where
  throwError = Err
  catchError (Ok x) _ = Ok x
  catchError (Err e) h = h e

(?/) :: MonadError String m => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return (x / y)

example :: Double -> Double -> Excep String
example x y = action `catchError` returnError
  where
    action = do
      q <- x ?/ y
      guard (q >= 0)
      if q > 100
        then do
          100 <- return q
          undefined
        else return $ show q

    returnError :: String -> Excep String
    returnError = Ok

-- task 11

data ParseError = ParseError { location :: Int, reason :: String }
  deriving (Eq, Show)

type ParseMonad = Either ParseError

parseHex :: String -> ParseMonad Integer
parseHex s = go s 0 0
  where
    go [] _ acc = Right acc
    go (c:cs) pos acc
      | isHexDigit c =
          let val = toInteger (digitToInt c)
          in go cs (pos + 1) (acc * 16 + val)
      | otherwise = Left $ ParseError (pos+1) (c : ": invalid digit")

printError :: ParseError -> ParseMonad String
printError (ParseError pos reason) = Right $ "At pos " ++ show pos ++ ": " ++ reason

test :: String -> String
test s = str
  where
    Right str = do
      n <- parseHex s
      return $ show n
      `catchError` printError

-- task 12

newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

instance Semigroup PwdError where
  (PwdError a) <> (PwdError b) = PwdError (a ++ "; " ++ b)

instance Monoid PwdError where
  mempty = PwdError ""

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  if length s < 8
    then liftIO (putStrLn "Incorrect input: password is too short!") >> throwError (PwdError "too short")
  else if not (any isNumber s)
    then liftIO (putStrLn "Incorrect input: password must contain some digits!") >> throwError (PwdError "no digits")
  else if not (any isPunctuation s)
    then liftIO (putStrLn "Incorrect input: password must contain some punctuations!") >> throwError (PwdError "no punctuation")
  else return s