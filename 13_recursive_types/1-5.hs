{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
import Data.List (sort)

newtype Fix f = In (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

out :: Fix f -> f (Fix f)
out (In x) = x

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata phi (In x) = phi (cata phi <$> x)

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana psi x = In (ana psi <$> psi x)

hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
hylo phi psi = cata phi . ana psi

-- TASK 1

data B x = Empty | Zero x | One x
    deriving (Eq,Show)

instance Functor B where
    fmap :: (a -> b) -> B a -> B b
    fmap _ Empty = Empty
    fmap f (Zero x) = Zero (f x)
    fmap f (One x) = One (f x)

type Bin = Fix B

phiB :: B Int -> Int
phiB Empty = 0
phiB (Zero x) = 2 * x
phiB (One x) = 2 * x + 1

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0 = Empty
psiB x =
    if even x then
        Zero (x `div` 2)
    else
        One (x `div` 2)

int2bin :: Int -> Bin
int2bin = ana psiB

-- TASK 2

data E e = Num Int | Add e e | Mult e e 

instance Functor E where
    fmap :: (a -> b) -> E a -> E b
    fmap _ (Num x) = Num x
    fmap f (Add e1 e2) = Add (f e1) (f e2)
    fmap f (Mult e1 e2) = Mult (f e1) (f e2)

type Expr = Fix E

phiE :: E Int -> Int
phiE (Num x) = x
phiE (Add x y) = x + y
phiE (Mult x y) = x * y

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num x) = show x
phiEShow (Add x y) = "(" ++ x ++ "+" ++ y ++ ")"
phiEShow (Mult x y) = "(" ++ x ++ "*" ++ y ++ ")"

-- TASK 3

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num x) = shows x
phiEShowS (Add e1 e2) = ("+ " ++) . e1 . (' ' :) . e2
phiEShowS (Mult e1 e2) = ("* " ++) . e1 . (' ' :) . e2

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add  (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num n) = push n
phiE' (Add f g) = add . g . f
phiE' (Mult f g) = mult . g . f

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'

-- TASK 4

data T a x = Leaf | Branch x a x
    deriving Eq

instance Functor (T a) where
    fmap :: (x -> b) -> T a x -> T a b
    fmap _ Leaf = Leaf
    fmap f (Branch a1 x a2) = Branch (f a1) x (f a2)

type Tree a = Fix (T a)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf = 0
phiTSum (Branch x y z) = x + y + z

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum

-- TASK 5

phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch l x r) = l ++ [x] ++ r

tree2listInorder :: Tree a -> [a] 
tree2listInorder = cata phiTInorder

psiTBST :: Ord a => Coalgebra (T a) [a] -- [a] -> T a [a] 
psiTBST [] = Leaf
psiTBST (x:xs) =
    let left  = [y | y <- xs, y < x]
        right = [y | y <- xs, y > x]
    in Branch left x right

list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTBST

sortL :: Ord a => [a] -> [a]
sortL = hylo phiTInorder psiTBST
