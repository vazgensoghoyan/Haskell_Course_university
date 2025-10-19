-- main data

data Tree a = Leaf | Node (Tree a) a (Tree a) 
    deriving Show

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
