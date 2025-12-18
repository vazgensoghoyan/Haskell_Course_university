

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

type TreeZ a = (a, CntxT a)

data CntxT a = CntxT (Tree a) (Tree a) [(Dir, a, Tree a)]
    deriving (Eq, Show)

data Dir = L | R deriving (Eq, Show)

mktz :: Tree a -> TreeZ a
mktz (Node left v right) = (v, CntxT left right [])

left :: TreeZ a -> TreeZ a
left (v, CntxT left right prevs) = 
    let (Node leftLeft leftV leftRight) = left
        cntxt = CntxT leftLeft leftRight ((L, v, right):prevs)
    in (leftV, cntxt)

right :: TreeZ a -> TreeZ a
right (v, CntxT left right prevs) = 
    let (Node rightLeft rightV rightRight) = right
        cntxt = CntxT rightLeft rightRight ((R, v, left):prevs)
    in (rightV, cntxt)

up :: TreeZ a -> TreeZ a
up = undefined
untz :: TreeZ a -> Tree a
untz = undefined
updTZ :: a -> TreeZ a -> TreeZ a
updTZ = undefined


