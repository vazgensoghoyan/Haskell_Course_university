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

newtype Cmps' f g x = Cmps' { getCmps' :: f (g x) }

instance (Functor f, Functor g) => Functor (Cmps' f g) where
  fmap :: (a -> b) -> Cmps' f g a -> Cmps' f g b
  fmap func (Cmps' cmps) = Cmps' ( fmap ( fmap func ) cmps )
  -- это доказательство того, что композиция функторов - функтор


-- task 3

infixl 4 >$<
infixl 4 >*<

(>$<) :: (a -> b) -> [a] -> [b]
f >$< xs = getZipList (f <$> ZipList xs)

(>*<) :: [a -> b] -> [a] -> [b]
fs >*< xs = getZipList (ZipList fs <*> ZipList xs)


-- task 4


data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where 
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (Tr x1 x2 x3) = Tr (f x1) (f x2) (f x3)

instance Applicative Triple where
  pure :: a -> Triple a
  pure x = Tr x x x

  (<*>) :: Triple (a -> b) -> Triple a -> Triple b
  (Tr f1 f2 f3) <*> (Tr x1 x2 x3) = Tr (f1 x1) (f2 x2) (f3 x3)


-- task 5

data Tree a = Nil | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Nil = Nil
  fmap f (Branch left value right) = Branch (fmap f left) (f value) (fmap f right)

instance Applicative Tree where
  pure :: a -> Tree a
  pure x = Branch (pure x) x (pure x)

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Branch l1 f r1) (Branch l2 x r2) = Branch (l1 <*> l2) (f x) (r1 <*> r2)


-- task 6

newtype Cmps f g x = Cmps { getCmps :: f (g x) }
  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap :: (a -> b) -> Cmps f g a -> Cmps f g b
  fmap func (Cmps cmps) = Cmps ( fmap ( fmap func ) cmps )

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure :: a -> Cmps f g a
  pure x = Cmps ( pure (pure x) )

  (<*>) :: Cmps f g (a -> b) -> Cmps f g a -> Cmps f g b
  (<*>) (Cmps fs) (Cmps xs) = Cmps ( (<*>) <$> fs <*> xs ) 


-- task 7

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> (divideList' xs)


-- task 8

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  fmap f (Arr2 g) = Arr2 (\x y -> f (g x y))

instance Functor (Arr3 e1 e2 e3) where
  fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  fmap f (Arr3 g) = Arr3 (\x y z -> f (g x y z))

instance Applicative (Arr2 e1 e2) where
  pure :: a -> Arr2 e1 e2 a
  pure x = Arr2 ( \i1 i2 -> x )

  (<*>) :: Arr2 e1 e2 (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  (<*>) (Arr2 f) (Arr2 x) = Arr2 ( \i1 i2 -> f i1 i2 $ x i1 i2 )

instance Applicative (Arr3 e1 e2 e3) where
  pure :: a -> Arr3 e1 e2 e3 a
  pure x = Arr3 ( \i1 i2 i3 -> x )

  (<*>) :: Arr3 e1 e2 e3 (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  (<*>) (Arr3 f) (Arr3 x) = Arr3 ( \i1 i2 i3 -> f i1 i2 i3 $ x i1 i2 i3 )

