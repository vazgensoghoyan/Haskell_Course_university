{-# LANGUAGE InstanceSigs #-}

-- task 1

data Result a = Ok a | Error String 
  deriving (Eq,Show)

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap _ (Error s) = Error s
    fmap f (Ok v) = Ok $ f v

instance Foldable Result where 
    foldr :: (a -> b -> b) -> b -> Result a -> b
    foldr _ x (Error s) = x
    foldr f x (Ok v) = f v x

instance Traversable Result where 
    traverse :: Applicative f => (a -> f b) -> Result a -> f (Result b)
    traverse _ (Error s) = pure (Error s)
    traverse f (Ok v) = fmap Ok (f v)
