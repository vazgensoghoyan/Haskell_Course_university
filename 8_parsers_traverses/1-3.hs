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

-- task 2

data NEList a = Single a | Cons a (NEList a)
    deriving (Eq,Show)

instance Functor NEList where
    fmap :: (a -> b) -> NEList a -> NEList b
    fmap f (Single x) = Single $ f x
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable NEList where
    foldMap :: Monoid m => (a -> m) -> NEList a -> m
    foldMap f (Single x) = f x
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable NEList where
    sequenceA :: Applicative f => NEList (f a) -> f (NEList a)
    sequenceA (Single x) = fmap Single x
    sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs
