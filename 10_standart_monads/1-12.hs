import Control.Monad.Writer

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
