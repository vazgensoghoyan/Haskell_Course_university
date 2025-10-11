import Data.List (union)

type Symb = String 

infixl 2 :@

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

-- TASK 1
freeVars :: Expr -> [Symb]
freeVars (Var s) = [s]
freeVars (t1 :@ t2) = freeVars t1 `union` freeVars t2
freeVars (Lam s expr) = filter (/= s) (freeVars expr) 

boundVars :: Expr -> [Symb]
boundVars (Var s) = []
boundVars (t1 :@ t2) = boundVars t1 ++ boundVars t2
boundVars (Lam s expr) = s : boundVars expr 

fresh :: Symb -> [Symb] -> Symb
fresh x used
    | x `elem` used = fresh (x ++ "'") used
    | otherwise     = x

subst :: Symb -> Expr -> Expr -> Expr 

subst v n m@(Var symb)
    | v == symb = n
    | otherwise = m
subst v n m@(expr1 :@ expr2) = 
    subst v n expr1 :@ subst v n expr2
subst v n m@(Lam x expr)
    | v == x = m
    | x `elem` freeVars n =
        let used  = freeVars expr ++ freeVars n
            x'    = fresh x used
            body' = subst x (Var x') expr
        in Lam x' (subst v n body')
    | otherwise = 
        Lam x (subst v n expr)

-- TASK 2

infix 1 `alphaEq`

alphaEq :: Expr -> Expr -> Bool
alphaEq e1 e2 = helper [] [] e1 e2
  where
    helper :: [Symb] -> [Symb] -> Expr -> Expr -> Bool
    helper env1 env2 (Var x) (Var y) =
      case (getIndex x env1, getIndex y env2) of
        (Just i1, Just i2) -> i1 == i2
        (Nothing, Nothing) -> x == y
        _ -> False
    helper env1 env2 (f1 :@ a1) (f2 :@ a2) = helper env1 env2 f1 f2 && helper env1 env2 a1 a2
    helper env1 env2 (Lam x e1) (Lam y e2) = helper (x:env1) (y:env2) e1 e2
    helper _ _ _ _ = False

    getIndex :: Eq a => a -> [a] -> Maybe Int
    getIndex x = helper' 0
      where
        helper' _ [] = Nothing
        helper' i (y:ys)
          | x == y    = Just i
          | otherwise = helper' (i+1) ys

-- TASK 3

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var s) = Nothing
reduceOnce (Lam s body) = fmap (Lam s) (reduceOnce body)
reduceOnce (Lam x body :@ arg) = Just (subst x arg body)
reduceOnce (f :@ a) = 
    case reduceOnce f of
        Just f' -> Just (f' :@ a)
        Nothing -> fmap (f :@) (reduceOnce a)

-- USING 3
-- TASK 4
-- TASK 5
-- TASK 6
