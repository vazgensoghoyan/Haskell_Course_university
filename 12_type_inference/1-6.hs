{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
import Data.List (union, nub, group)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Maybe (fromMaybe)
import qualified Data.Bifunctor

-- GIVEN

infixl 4 :@
infixr 3 :->

type Symb = String

-- Терм
data Expr = Var Symb | Expr :@ Expr | Lam Symb Expr
    deriving (Eq,Show)

-- Тип
data Type = TVar Symb | Type :-> Type
    deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
    deriving (Eq,Show)

-- Подстановка
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

fresh :: Symb -> [Symb] -> Symb
fresh x used
    | x `elem` used = fresh (x ++ "'") used
    | otherwise = x

equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]

equations (Env env) (Var s) t =
    case lookup s env of
        Nothing -> throwError $ "There is no variable \"" ++ s ++ "\" in the environment."
        Just st -> return [(t, st)]

equations env (f :@ x) t =
    let freshAlpha = TVar $ fresh "a" (freeTVarsEnv env)
    in do
        eq1 <- equations env f (freshAlpha :-> t)
        eq2 <- equations env x freshAlpha
        return $ eq1 ++ eq2

equations (Env env) (Lam s body) t =
    let freshAlpha = TVar $ fresh "a" (freeTVarsEnv (Env env))
        freshBeta = TVar $ fresh "b" (freeTVarsEnv (Env env))
        env' = Env ((s, freshAlpha) : env)
    in do
        eqs <- equations env' body freshBeta
        return ((freshAlpha :-> freshBeta, t) : eqs)

squeezeEqs :: [(Type,Type)] -> (Type,Type)
squeezeEqs [(t1,t2)] = (t1,t2)
squeezeEqs ((a1,a2):(b1,b2):xs) = squeezeEqs ((a1 :-> b1, a2 :-> b2):xs)

principalPair :: MonadError String m => Expr -> m (Env,Type)
principalPair expr = do
    let fvExpr = freeVars expr
    let env = fmap (\x -> (x, TVar x)) fvExpr
    let used = freeTVarsEnv (Env env)
    let t = fresh "a" used
    eqs <- equations (Env env) expr (TVar t)
    let (t1,t2) = squeezeEqs eqs
    s <- unify t1 t2
    return (appSubsEnv s (Env env), appSubsTy s (TVar t))
