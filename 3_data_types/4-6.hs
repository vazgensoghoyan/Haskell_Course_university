-- 4
data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error   Error   = EQ
cmp Warning Warning = EQ
cmp Info    Info    = EQ
cmp Error   _       = GT
cmp _       Error   = LT
cmp Warning Info    = GT
cmp Info    Warning = LT

-- 5
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p = case firstName p of
    (x:_:_) -> p { firstName = x : "." }
    _       -> p

-- 6
data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node l v r) = v + treeSum l + treeSum r

treeHeight :: Tree a -> Int
treeHeight Leaf = 1
treeHeight (Node l _ r) = 1 + max (treeHeight l) (treeHeight r)
