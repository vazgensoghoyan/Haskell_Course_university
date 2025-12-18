

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

type TreeZ a = (a, CntxT a)

data CntxT a = CntxT (Tree a) (Tree a) [(Dir, a, Tree a)]
    deriving (Eq, Show)

data Dir = L | R deriving (Eq, Show)

-- TASK 1

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
up (v, CntxT left right (prev:prevs))
    | (L, prevV, prevRight) <- prev = 
        let newLeft = Node left v right
            cntxt = CntxT newLeft prevRight prevs
        in (prevV, cntxt)
    | (r, prevV, prevLeft) <- prev =
        let newRight = Node left v right
            cntxt = CntxT prevLeft newRight prevs
        in (prevV, cntxt)

untz :: TreeZ a -> Tree a
untz (v, CntxT left right []) = Node left v right
untz treeZ = untz $ up treeZ

updTZ :: a -> TreeZ a -> TreeZ a
updTZ newVal (_, cntxt) = (newVal, cntxt)

-- TASK 2

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

_1my :: Lens (a, b) a
_1my ret (a,b) = fmap (,b) (ret a)

_2my :: Lens (a, b) b
_2my ret (a,b) = fmap (a,) (ret b)

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens get set ret s = fmap (set s) (ret $ get s)

_1 :: Lens (a,b) a
_1 = lens fst (\(_,y) v -> (v,y))

_2 :: Lens (a,b) b
_2 = lens snd (\(x,_) v -> (x,v))

newtype Const a x = Const {getConst :: a}

instance Functor (Const c) where
    fmap :: (a -> b) -> Const c a -> Const c b
    fmap _ (Const v) = Const { getConst = v }

view :: Lens s a -> s -> a
view lns s = getConst (lns Const s)

newtype Identity a = Identity {runIdentity :: a}
instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = Identity { runIdentity = f x }

over :: Lens s a -> (a -> a) -> s -> s
over lns fn s = runIdentity $ lns (Identity . fn) s

set :: Lens s a -> a -> s -> s
set lns a = over lns (const a)
