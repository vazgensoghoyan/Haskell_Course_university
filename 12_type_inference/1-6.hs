{-# LANGUAGE FlexibleContexts #-}
import Data.List (union, nub)
import Control.Monad.Error.Class (MonadError (throwError))

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
