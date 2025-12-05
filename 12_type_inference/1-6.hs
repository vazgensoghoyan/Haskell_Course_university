{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
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

equations env expr t = 
    let used = freeTVarsEnv env ++ freeTVars t
    in go used env expr t
    where
        go :: MonadError String m => [Symb] -> Env -> Expr -> Type -> m [(Type,Type)]
        go used env (Var x) t = do
            tx <- appEnv env x
            return [(t, tx)]
        go used env (f :@ x) t = do
            let new = fresh used
            let newT = TVar new
            eqs1 <- go (new:used) env x newT
            eqs2 <- go (new:used) env f (newT :-> t)
            return $ eqs1 ++ eqs2
        go used env (Lam arg body) t = do
            let alpha = fresh used
            let beta = fresh (alpha : used)
            let newUsed = alpha:beta:used
            eqs <- go newUsed (extendEnv env arg (TVar alpha)) body (TVar beta)
            return $ (TVar alpha :-> TVar beta, t) : eqs

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

pP :: Expr -> Either String (Env, Type)
pP expr = do
    (env, ty) <- principalPair expr
    let fv = freeTVarsEnv env `union` freeTVars ty
    let varsStorage = gaz ++ [s ++ show d | d <- [1 ..], s <- gaz] where gaz = group ['a'..'y'] 
    let sub = SubsTy $ zip fv (map TVar varsStorage)
    return (appSubsEnv sub env, appSubsTy sub ty)
