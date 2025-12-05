{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List (union, nub, group)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Maybe (fromMaybe)
import qualified Data.Bifunctor
import Control.Monad.State
import qualified Data.Set as Set
import Control.Monad (foldM)

-- GIVEN

infixl 4 :@
infixr 3 :->

type Symb = String

data Expr = Var Symb | Expr :@ Expr | Lam Symb Expr
    deriving (Eq,Show)

data Type = TVar Symb | Type :-> Type
    deriving (Eq,Show)

newtype Env = Env [(Symb,Type)]
    deriving (Eq,Show)

newtype SubsTy = SubsTy [(Symb, Type)]
    deriving (Eq,Show)

-- TASK 1

freeVars :: Expr -> [Symb]
freeVars (Var s) = [s]
freeVars (t1 :@ t2) = freeVars t1 `union` freeVars t2
freeVars (Lam s expr) = filter (/= s) (freeVars expr)

freeTVars :: Type -> [Symb]
freeTVars (TVar s) = [s]
freeTVars (t1 :-> t2) = freeTVars t1 `union` freeTVars t2

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) s t = Env $ (s,t):env

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = nub $ do
    (_, t) <- env
    freeTVars t

-- TASK 2

lookupT :: [(Symb, Type)] -> Symb -> Maybe Type
lookupT [] _ = Nothing
lookupT ((x,t):xs) s
    | x == s = Just t
    | otherwise = lookupT xs s

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = case lookupT xs v of
    Nothing -> throwError $ "There is no variable \"" ++ v ++ "\" in the environment."
    Just t -> return t

-- TASK 3

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy s) (TVar v) = fromMaybe (TVar v) (lookup v s)
appSubsTy s (t1 :-> t2) = appSubsTy s t1 :-> appSubsTy s t2

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv s (Env env) = Env $ fmap (Data.Bifunctor.second (appSubsTy s)) env

-- TASK 4

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy (SubsTy s1) (SubsTy s2) =
    let s2' = [(x, appSubsTy (SubsTy s1) t) | (x,t) <- s2]
        s1' = [(x, t) | (x,t) <- s1, x `notElem` map fst s2]
    in SubsTy (s2' ++ s1')

instance Semigroup SubsTy where
    (<>) :: SubsTy -> SubsTy -> SubsTy
    (<>) = composeSubsTy

instance Monoid SubsTy where
    mempty :: SubsTy
    mempty = SubsTy []

    mappend :: SubsTy -> SubsTy -> SubsTy
    mappend = composeSubsTy

-- TASK 5

errorText :: Type -> Type -> String
errorText t1 t2 = "Can't unify (" ++ show t1 ++ ") with " ++ show t2 ++ "!"

unify :: MonadError String m => Type -> Type -> m SubsTy

unify t1@(TVar x) t2@(TVar y)
    | x == y = return $ SubsTy []
    | otherwise = return $ SubsTy [(x, t2)]

unify t1@(TVar x) t2 =
    if x `elem` freeTVars t2
        then
            throwError $ errorText t1 t2
        else
            return $ SubsTy [(x, t2)]

unify t1 t2@(TVar x) = unify t2 t1

unify (a1 :-> a2) (b1 :-> b2) = do
    u2 <- unify a2 b2
    let a1' = appSubsTy u2 a1
    let b1' = appSubsTy u2 b1
    u1 <- unify a1' b1'
    return $ u1 <> u2

-- TASK 6

fresh :: [Symb] -> Symb
fresh used = head $ filter (`notElem` used) candidates
    where
        candidates = [ c : n | n <- "" : map show [1..], c <- ['a'..'z'] ]

equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]
equations env expr t = do
    let used = freeTVarsEnv env ++ freeTVars t
    res <- go used env expr t
    return $ snd res
    where
        go :: MonadError String m => [Symb] -> Env -> Expr -> Type -> m ([Symb], [(Type,Type)])
        go used env (Var x) t = do
            tx <- appEnv env x
            let newUsed = freeTVars t `union` freeTVars tx
            return (used `union` newUsed, [(t, tx)])
        go used env (f :@ x) t = do
            let new = fresh used
            let newT = TVar new
            (u1, eqs1) <- go (new:used) env f (newT :-> t)
            (u2, eqs2) <- go (new:u1) env x newT
            let newUsed = new : u1 `union` u2 `union` used
            return (newUsed, eqs1 ++ eqs2)
        go used env (Lam arg body) t = do
            let alpha = fresh used
            let beta = fresh (alpha : used)
            let newUsed = alpha:beta:used
            (u, eqs) <- go newUsed (extendEnv env arg (TVar alpha)) body (TVar beta)
            let resEqs = (TVar alpha :-> TVar beta, t) : eqs
            return (newUsed `union` u, resEqs)

generateFreshTypes :: [Symb] -> [(Symb, Type)]
generateFreshTypes vars = go vars []
    where
        go [] _ = []
        go (v:vs) used =
            let tv = fresh used
            in (v, TVar tv) : go vs (tv : used)

principalPair :: MonadError String m => Expr -> m (Env,Type)
principalPair expr = do
    let env = Env $ generateFreshTypes $ freeVars expr
    let used = freeTVarsEnv env
    let t0 = TVar $ fresh used
    eqs <- equations env expr t0
    s <- foldl
            ( \acc (t1, t2) -> do
                s1 <- acc
                s2 <- unify (appSubsTy s1 t1) (appSubsTy s1 t2)
                return $ s2 <> s1 )
           (return mempty)
           eqs
    return (appSubsEnv s env, appSubsTy s t0)

-- my additional convenient functions

eqEither :: Expr -> Either String [(Type,Type)]
eqEither expr = do
    let env = Env $ generateFreshTypes $ freeVars expr
    let used = freeTVarsEnv env
    let t0 = TVar $ fresh used
    equations env expr t0

ppEither :: Expr -> Either String (Env, Type)
ppEither = principalPair

ppBeautiful :: Expr -> Either String (Env, Type)
ppBeautiful expr = do
    (env, ty) <- principalPair expr
    let fv = freeTVarsEnv env `union` freeTVars ty
    let varsStorage = gaz ++ [s ++ show d | d <- [1 ..], s <- gaz] where gaz = group ['a'..'y']
    let sub = SubsTy $ zip fv (map TVar varsStorage)
    return (appSubsEnv sub env, appSubsTy sub ty)

-- TEST DATA

-- компактно записанные переменные 
[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] = map (Var . (:[])) "abcdefghijklmnopqrstuvwxyz"
-- аппликация двух аргументов
app2 f x y = f :@ x :@ y
-- аппликация трёх аргументов
app3 f x y z = f :@ x :@ y :@ z

-- комбинаторы
cI     = Lam "x" x
cK     = Lam "x" $ Lam "y" x
cK_ast = Lam "x" $ Lam "y" y
cB     = Lam "f" $ Lam "g" $ Lam "x" $ f :@ (g :@ x)
cS     = Lam "f" $ Lam "g" $ Lam "x" $ f :@ x :@ (g :@ x)

-- Булевы значения
fls = Lam "t" $ Lam "f" f
tru = Lam "t" $ Lam "f" t

iif = Lam "b" $ Lam "x" $ Lam "y" $ b :@ x :@ y

not' = Lam "b" $ Lam "t" $ Lam "f" $ app2 b f t
and' = Lam "x" $ Lam "y" $ app2 x y fls
or'  = Lam "x" $ Lam "y" $ app2 x tru y

-- пары
pair = Lam "x" $ Lam "y" $ Lam "f" $ app2 f x y

fst' = Lam "p" $ p :@ tru
snd' = Lam "p" $ p :@ fls

-- числа Чёрча
zero  = Lam "s" $ Lam "z" z
one   = Lam "s" $ Lam "z" $ s :@ z
two   = Lam "s" $ Lam "z" $ s :@ (s :@ z)
three = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ z))
four  = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ z)))
five  = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ z))))
six   = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z)))))
seven = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z))))))
eight = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z)))))))
nine  = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z))))))))
ten   = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z)))))))))

iszro = Lam "n" $ n :@ Lam "x" fls :@ tru

suc =  Lam "n" $ Lam "s" $ Lam "z" $ s :@ (n :@ s :@ z)
suc' = Lam "n" $ Lam "s" $ Lam "z" $ n :@ s :@ (s :@ z)

plus  = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ app2 m s (app2 n s z)

mult = Lam "m" $ Lam "n" $ Lam "s" $ m :@ (n :@ s)

pow = Lam "m" $ Lam "n" $ n :@ m

omega = Lam "x" $ x :@ x

zp = pair :@ zero :@ zero
sp = Lam "p" $  pair :@ (snd' :@ p) :@ (suc :@ (snd' :@ p))
pred' = Lam "m" $ fst' :@ (m :@ sp :@ zp)

-- факториал
zf = pair :@ one :@ zero
sf = Lam "p" $ pair :@ (mult :@ (fst' :@  p) :@ (suc :@ (snd' :@ p))) :@ (suc :@ (snd' :@ p))
fac = Lam "m" $ fst' :@ (m :@ sf :@ zf)

-- общая схема примитивной рекурсии
xz = Lam "x" $ pair :@ x :@ zero
fs = Lam "f" $ Lam "p" $ pair :@ (f :@ (fst' :@ p) :@ (snd' :@ p)) :@ (suc :@ (snd' :@ p))
rec = Lam "m" $ Lam "f" $ Lam "x" $ fst' :@ (m :@ (fs :@ f) :@ (xz :@ x))

pred'' = Lam "n" $ rec :@ n :@ cK_ast :@ zero

minus = Lam "k" $ Lam "l" $ l :@ pred' :@ k

lt = Lam "n" $ Lam "m" $ not' :@ (iszro :@ (minus :@ m :@ n))
ge = Lam "n" $ Lam "m" $  iszro :@ app2 minus m n
gt = Lam "n" $ Lam "m" $ not' :@ (iszro :@ app2 minus n m)
le = Lam "n" $ Lam "m" $  iszro :@ app2 minus n m
eq = Lam "n" $ Lam "m" $ and' :@ (le :@ m :@ n) :@ (ge :@ m :@ n)

fac'step = Lam "u" $ Lam "v" $ app2 mult u (suc :@ v)
fac' = Lam "n" $ app3 rec n fac'step one

-- Y-комбинатор
cY = Lam "f" $ Lam "z" (f :@ (z :@ z)) :@ Lam "z" (f :@ (z :@ z))
cTheta = aa :@ aa
    where aa = Lam "a" $ Lam "b" $ b :@ (a :@ a :@ b)

fac''step = Lam "f" $ Lam "n" $ iif :@ (iszro :@ n) :@ one :@ (mult :@ n :@ (f :@ (pred' :@ n)))
fac''  =  cY :@ fac''step
fac''' = cTheta :@ fac''step

-- списки
nil  = Lam "c" $ Lam "n" n
cons = Lam "e" $ Lam "l" $ Lam "c" $ Lam "n" $ app2 c e (app2 l c n)

l532 = app2 cons five (app2 cons three (app2 cons two nil))
l2 = Lam "c" $ Lam "n" $ c :@ two :@ n

empty = Lam "l" $ app2 l (Lam "h" $ Lam "t" fls) tru

length' = Lam "l" $ app2 l (Lam "x" $ Lam "y" $ suc :@ y) zero
length'' = Lam "l" $ app2 l (Lam "y" $ suc) zero

head' = Lam "l" $ app2 l cK cI

zpl = app2 pair nil nil
spl = Lam "e" $ Lam "p" $ app2 pair (snd' :@ p) (app2 cons e (snd' :@ p))
tail' = Lam "l" $ fst' :@ app2 l spl zpl

sum' = Lam "l" $ app2 l plus zero
sum'' = Lam "l" $ app2 l (Lam "h" $ Lam "t" $ app2 plus h t) zero

-- TESTS

{-

GHCi> term = Lam "y" $ Var "x"
GHCi> env = Env [("x",TVar "a" :-> TVar "b")]
GHCi> let Right eqs = equations env term (TVar "o") in eqs
[(TVar "d",TVar "a" :-> TVar "b"),(TVar "c" :-> TVar "d",TVar "o")]
GHCi> let Left err = equations (Env []) term (TVar "o") in err
"There is no variable \"x\" in the environment."

GHCi> let Right pp = principalPair (Var "x") in pp
(Env [("x",TVar "a")],TVar "a")
GHCi> let Right pp = principalPair (Var "f" :@ Var "x") in pp
(Env [("f",TVar "a" :-> TVar "b"),("x",TVar "a")],TVar "b")
GHCi> let Right pp = principalPair (Lam "x" $ Lam "y" $ Var "y") in pp
(Env [],TVar "a" :-> (TVar "b" :-> TVar "b"))
GHCi> let Left err = principalPair (Var "x" :@ Var "x") in err
"Can't unify (TVar \"a\") with (TVar \"a\" :-> TVar \"b\")!"

-}