{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative (ZipList(ZipList), getZipList)


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

newtype Cmps f g x = Cmps { getCmps :: f (g x) }

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap :: (a -> b) -> Cmps f g a -> Cmps f g b
  fmap func (Cmps cmps) = Cmps ( fmap ( fmap func ) cmps )
  -- это доказательство того, что композиция функторов - функтор


-- task 3

infixl 4 >$<
infixl 4 >*<

(>$<) :: (a -> b) -> [a] -> [b]
f >$< xs = getZipList (f <$> ZipList xs)

(>*<) :: [a -> b] -> [a] -> [b]
fs >*< xs = getZipList (ZipList fs <*> ZipList xs)
