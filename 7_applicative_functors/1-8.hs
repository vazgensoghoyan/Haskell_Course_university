{-# LANGUAGE InstanceSigs #-}


-- task 1
data E l r = L l | R r
    deriving (Eq, Show)

instance Functor (E l) where
  fmap :: (a -> b) -> E l a -> E l b
  fmap f (L x) = L x
  fmap f (R x) = R (f x)

instance Applicative (E l) where
  pure :: a -> E l a
  pure = R

  (<*>) :: E l (a -> b) -> E l a -> E l b
  L a <*> _ = L a
  R _ <*> L x = L x
  R f <*> R x = R (f x)
  

-- task 2
