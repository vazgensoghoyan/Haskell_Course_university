import Data.List (union)

-- 1
type Symb = String 

infixr 3 :->
data Type = TVar Symb | Type :-> Type
    deriving (Eq,Show)

arity :: Type -> Int
arity (TVar _) = 0
arity (t1 :-> t2) = 1 + arity t2

order :: Type -> Int
order (TVar _) = 0
order (t1 :-> t2) = max (order t1 + 1) (order t2)

-- 2
infixl 4 :@
data Term = Var Symb | Term :@ Term | Lam Symb Type Term
    deriving (Eq,Show)

freeVars :: Term -> [Symb]
freeVars (Var s) = [s]
freeVars (t1 :@ t2) = freeVars t1 `union` freeVars t2
freeVars (Lam s _ trm) = filter (/= s) (freeVars trm) 

boundVars :: Term -> [Symb]
boundVars (Var s) = []
boundVars (t1 :@ t2) = boundVars t1 ++ boundVars t2
boundVars (Lam s _ trm) = s : boundVars trm 

-- 3
type Env = [(Symb,Type)]

typeFromEnv :: Env -> Symb -> Maybe Type
typeFromEnv [] s = Nothing
typeFromEnv ((s1,t):xs) s2 = if s1 == s2 then Just t else typeFromEnv xs s2

infer0 :: Term -> Maybe Type
infer0 = infer [] 

infer :: Env -> Term -> Maybe Type
infer env (Var s) = typeFromEnv env s
infer env (t1 :@ t2) = 
    case infer env t1 of
        Nothing -> Nothing
        Just (t11 :-> t12) -> 
            case infer env t2 of
                Nothing -> Nothing
                Just t2' -> 
                    if t11 == t2' then Just t12 else Nothing
        Just _ -> Nothing
infer env (Lam s tp trm) = 
    case infer ((s,tp):env) trm of
        Nothing -> Nothing
        Just t -> Just (tp :-> t)
