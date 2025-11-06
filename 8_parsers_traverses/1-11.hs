{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
import Data.Char

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

newtype Parser tok a =
  Parser { runParser :: [tok] ->  Maybe ([tok],a) }

charA :: Parser Char Char
charA = Parser f where
  f (c:cs) | c == 'A' = Just (cs,c)
  f _                 = Nothing

{-
GHCi> runParser charA "ABC"
Just ('A',"BC")
GHCi> runParser charA "BCD"
Nothing
-}

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
  f (c:cs) | pr c  = Just (cs,c)
  f _              = Nothing

{-
GHCi> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
GHCi> runParser (satisfy isLower) "ABC"
Nothing
-}

lower :: Parser Char Char
lower = satisfy isLower

char :: Char -> Parser Char Char
char c = satisfy (== c)

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit
-- для этого
instance Functor (Parser tok) where
  fmap :: (a -> b) -> Parser tok a -> Parser tok b
  fmap g (Parser p) = Parser $ (fmap . fmap . fmap) g p
{-
GHCi> runParser digit "12AB"
Just ("2AB",1)
GHCi> runParser digit "AB12"
Nothing
-}

{-
СЕМАНТИКА
pure: парсер, всегда возвращающий заданное значение;
(<*>): нужно получить результаты первого парсера, затем
второго, а после этого применить первые ко вторым.
-}
instance Applicative (Parser tok) where
  pure :: a -> Parser tok a
  pure x = Parser $ \s -> Just (s, x)
  (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
  Parser u <*> Parser v = Parser f where
    f xs = case u xs of 
      Nothing       -> Nothing
      Just (xs', g) -> case v xs' of 
        Nothing        -> Nothing
        Just (xs'', x) -> Just (xs'', g x)

{-
GHCi> runParser (pure (,) <*> digit <*> digit) "12AB"
Just ("AB",(1,2))
GHCi> runParser (pure (,) <*> digit <*> digit) "1AB2"
Nothing
-}

multiplication :: Parser Char Int
multiplication = (*) <$> digit <* char '*' <*> digit
{-
GHCi>  runParser multiplication "6*7"
Just ("",42)
-}

-- Альтернативы

{-
СЕМАНТИКА
empty - парсер, всегда возвращающий неудачу;
<|> - пробуем первый, при неудаче пробуем второй на исходной строке.
-}

instance Alternative (Parser tok) where
  empty :: Parser tok a
  empty = Parser $ const Nothing
  (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  Parser u <|> Parser v = Parser f where 
    f xs = case u xs of
      Nothing -> v xs
      z       -> z

-- END OF CODE FOR THE TASK

-- THE TASK

nat :: Parser Char Int
nat = go <$> some digit
    where 
        go :: Num a => [a] -> a
        go = foldl (\a b -> 10 * a + b) 0

-- task 4

