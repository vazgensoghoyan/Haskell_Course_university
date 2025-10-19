-- main data

data Tree a = Leaf | Node (Tree a) a (Tree a) 

-- for tests

t1 = Node (Node (Node Leaf 5 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)
t2 = Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)
t3 = Node Leaf 42 Leaf
tInf1 n = Node (tInf1 (n+2)) n (Node Leaf 42 Leaf)
tInf2 n = Node (tInf2 (n+2)) n (tInf2 (3*n-1))

-- task 1
elemTree :: Eq a => a -> Tree a -> Bool
elemTree el tr = helper [tr] el
    where
        helper :: Eq a => [Tree a] -> a -> Bool
        helper [] _ = False
        helper (Leaf:rem) el = helper rem el
        helper ((Node l val r):rem) el 
            | el == val = True
            | otherwise = helper (rem ++ [l, r]) el 

-- task 2
instance Eq a => Eq (Tree a) where 
    x == y = helper [x] [y] 
        where
            helper :: Eq a => [Tree a] -> [Tree a] -> Bool
            helper [] [] = True
            helper (Leaf:xs) (Leaf:ys) = helper xs ys
            helper ((Node l1 v1 r1):xs) ((Node l2 v2 r2):ys) = 
                v1 == v2 && helper (xs ++ [l1, r1]) (ys ++ [l2, r2])
            helper _ _ = False

-- task 3
instance Functor Tree where 
    fmap f Leaf = Leaf
    fmap f (Node left val right) = Node (fmap f left) (f val) (fmap f right)

-- task 4
instance Show a => Show (Tree a) where 
    show Leaf = "{}"
    show (Node right val left) =
        "<" ++ show right ++ show val ++ show left ++ ">"

-- task 5
instance Read a => Read (Tree a) where
    readsPrec _ s =
        case s of
            ('{':'}':rest) ->
                [(Leaf, rest)]
            ('<':rest) ->
                [ (Node left val right, right_rest)
                    | (left, left_rest) <- reads rest
                    , (val :: a, val_rest)  <- reads left_rest
                    , (right, '>':right_rest) <- reads val_rest
                ]
            _ -> []
