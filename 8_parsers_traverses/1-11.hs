{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
import Data.Char
import Data.Traversable (fmapDefault, foldMapDefault)

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

-- task 3

-- SOME CODE FOR THE TASK

newtype Parser' tok a = Parser' { runParser' :: [tok] ->  Maybe ([tok],a) }

charA :: Parser' Char Char
charA = Parser' f where
    f (c:cs) | c == 'A' = Just (cs,c)
    f _                 = Nothing

{-
GHCi> runParser' charA "ABC"
Just ('A',"BC")
GHCi> runParser' charA "BCD"
Nothing
-}

satisfy :: (tok -> Bool) -> Parser' tok tok
satisfy pr = Parser' f where
    f (c:cs) | pr c  = Just (cs,c)
    f _              = Nothing

{-
GHCi> runParser' (satisfy isUpper) "ABC"
Just ('A',"BC")
GHCi> runParser' (satisfy isLower) "ABC"
Nothing
-}

lower :: Parser' Char Char
lower = satisfy isLower

char :: Char -> Parser' Char Char
char c = satisfy (== c)

digit :: Parser' Char Int
digit = digitToInt <$> satisfy isDigit
-- для этого
instance Functor (Parser' tok) where
    fmap :: (a -> b) -> Parser' tok a -> Parser' tok b
    fmap g (Parser' p) = Parser' $ (fmap . fmap . fmap) g p
{-
GHCi> runParser' digit "12AB"
Just ("2AB",1)
GHCi> runParser' digit "AB12"
Nothing
-}

{-
СЕМАНТИКА
pure: парсер, всегда возвращающий заданное значение;
(<*>): нужно получить результаты первого парсера, затем
второго, а после этого применить первые ко вторым.
-}
instance Applicative (Parser' tok) where
    pure :: a -> Parser' tok a
    pure x = Parser' $ \s -> Just (s, x)
    (<*>) :: Parser' tok (a -> b) -> Parser' tok a -> Parser' tok b
    Parser' u <*> Parser' v = Parser' f where
        f xs = case u xs of
            Nothing       -> Nothing
            Just (xs', g) -> case v xs' of
                Nothing        -> Nothing
                Just (xs'', x) -> Just (xs'', g x)

{-
GHCi> runParser' (pure (,) <*> digit <*> digit) "12AB"
Just ("AB",(1,2))
GHCi> runParser' (pure (,) <*> digit <*> digit) "1AB2"
Nothing
-}

multiplication :: Parser' Char Int
multiplication = (*) <$> digit <* char '*' <*> digit
{-
GHCi>  runParser' multiplication "6*7"
Just ("",42)
-}

-- Альтернативы

{-
СЕМАНТИКА
empty - парсер, всегда возвращающий неудачу;
<|> - пробуем первый, при неудаче пробуем второй на исходной строке.
-}

instance Alternative (Parser' tok) where
    empty :: Parser' tok a
    empty = Parser' $ const Nothing
    (<|>) :: Parser' tok a -> Parser' tok a -> Parser' tok a
    Parser' u <|> Parser' v = Parser' f where
        f xs = case u xs of
            Nothing -> v xs
            z       -> z

-- END OF CODE FOR THE TASK

-- THE TASK

nat :: Parser' Char Int
nat = go <$> some digit
    where
        go :: Num a => [a] -> a
        go = foldl (\a b -> 10 * a + b) 0

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

instance Foldable Triple where
    foldr :: (a -> b -> b) -> b -> Triple a -> b
    foldr f x (Tr x1 x2 x3) = x1 `f` (x2 `f` (x3 `f` x))

instance Traversable Triple where
    sequenceA :: Applicative f => Triple (f a) -> f (Triple a)
    sequenceA (Tr x1 x2 x3) = Tr <$> x1 <*> x2 <*> x3

-- task 5

data Tree a = Nil | Branch (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap = fmapDefault

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap = foldMapDefault

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse _ Nil = pure Nil
    traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r

-- task 6

newtype Cmps f g x = Cmps { getCmps :: f (g x) }
    deriving (Eq,Show)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
    foldMap :: Monoid m => (a -> m) -> Cmps f g a -> m
    foldMap h v = foldMap (foldMap h) (getCmps v)

-- task 7

instance (Functor f, Functor g) => Functor (Cmps f g) where
    fmap :: (Functor f, Functor g) => (a -> b) -> Cmps f g a -> Cmps f g b
    fmap f (Cmps v) = Cmps $ fmap (fmap f) v

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
    pure :: (Applicative f, Applicative g) => a -> Cmps f g a
    pure x = Cmps (pure (pure x))

    (<*>) :: (Applicative f, Applicative g) => Cmps f g (a -> b) -> Cmps f g a -> Cmps f g b
    (Cmps x) <*> (Cmps y) = Cmps $ (<*>) <$> x <*> y

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
    traverse :: (Traversable f, Traversable g, Applicative h) =>
        (a -> h b) -> Cmps f g a -> h (Cmps f g b)
    traverse func x = Cmps <$> traverse sequenceA (getCmps (fmap func x))

-- task 8

newtype Parser a = Parser { apply :: String -> [(a, String)] }

parse :: Parser a -> String -> [a]
parse p = map fst . filter (null . snd) . apply p

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser g) = Parser $ fmap (\(a,b) -> (f a, b)) . g

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> [(x, s)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser f) <*> (Parser x) = Parser $ \s ->
        [ (f a, s'') | (f, s')  <- f s , (a, s'') <- x s']

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const []

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser f) <|> (Parser g) = Parser $ \s -> f s ++ g s

-- DATA FOR NEXT TASK

class Functor f => Monoidal f where
    unit  :: f ()
    (*&*) :: f a -> f b -> f (a,b)

instance Monoidal [] where
    unit :: [()]
    unit = [()]
    (*&*) :: [a] -> [b] -> [(a, b)]
    xs *&* ys = [ (x,y) | x <- xs, y <- ys ]
  
instance Monoidal ZipList where
    unit :: ZipList ()
    unit = ZipList (repeat ())
    (*&*) :: ZipList a -> ZipList b -> ZipList (a, b)
    (ZipList xs) *&* (ZipList ys) = ZipList (zip xs ys)

-- END OF DATA FOR NEXT TASK

-- task 9

instance Monoidal Maybe where
    unit :: Maybe ()
    unit = Just ()

    (*&*) :: Maybe a -> Maybe b -> Maybe (a, b)
    (Just x) *&* (Just y) = Just (x, y)
    _ *&* _ = Nothing 

instance Monoid s => Monoidal ((,) s) where
    unit :: Monoid s => (s, ())
    unit = (mempty, ())

    (*&*) :: Monoid s => (s, a) -> (s, b) -> (s, (a, b))
    (s1, x) *&* (s2, y) = (s1 <> s2, (x, y))

instance Monoidal ((->) e) where
    unit :: e -> ()
    unit = const ()

    (*&*) :: (e -> a) -> (e -> b) -> e -> (a, b)
    f *&* g = \x -> (f x, g x)

-- task 10

unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a,b)
pair' x y = (,) <$> x <*> y

-- task 11

pure' :: Monoidal f => a -> f a
pure' x = fmap (const x) unit

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' x y = fmap (\(a, b) -> a b) (x *&* y)
