{-# LANGUAGE FlexibleInstances #-}

import Data.List ( isPrefixOf, union )

import Text.Read
import Data.Text (strip)
import Data.Char (isSpace)

type Symb = String

infixl 2 :@

data Expr = Var Symb | Expr :@ Expr | Lam Symb Expr
    deriving Eq

-- DATA

-- компактно записанные переменные 
[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] = map (Var . (:[])) "abcdefghijklmnopqrstuvwxyz"

-- Некоторые составные выражения
expr1 = a
expr2 = Lam "x" a
expr3 = Lam "x" (a :@ b)
expr4 = a :@ b
expr5 = a :@ b :@ c
expr6 = Lam "x" (Lam "y" (x :@ y :@ z))
expr7 = Lam "x" (x :@ x) :@ Lam "y" (y :@ y)
expr8 = Lam "f" (Lam "x" (f :@ (x :@ x)) :@ Lam "x" (f :@ (x :@ x)))


-- TASK 6

instance Show Expr where
    showsPrec _ (Var x) = showString x

    showsPrec d (Lam x e) =
        showParen (d > 0) $
            showString "\\" . showString x . showString " -> " . shows e

    showsPrec d e = showParen (d > 2) (showsApp e)
        where
            flattenApp :: Expr -> [Expr]
            flattenApp (f :@ a) = flattenApp f ++ [a]
            flattenApp x = [x]

            showsApp :: Expr -> ShowS
            showsApp expr =
                let args = flattenApp expr
                in case args of
                    []     -> id
                    (h:ts) -> showsPrec 3 h . foldl (\acc a -> acc . showString " " . showArg a) id ts

            showArg :: Expr -> ShowS
            showArg v@(Var _) = showsPrec 3 v
            showArg lam@(Lam _ _) = showParen True (shows lam)
            showArg app@(_ :@ _) = showParen True (showsPrec 2 app)


instance Read Expr where
    readsPrec _ s = [(readApps s, "")]

readApps :: String -> Expr
readApps s = foldl1 (:@) (readExpr s)

readExpr :: String -> [Expr]
readExpr "" = error "readExpr: пустая строка"
readExpr s@(c:_)
    | c == ' '  = readExpr $ trim s
    | c == '\\' = [readLam s]
    | c == '('  =
        let (body, rem) = getParens s
        in case trim rem of
            "" -> [readApps body]
            _ -> readExpr body ++ readExpr rem
    | otherwise =
        let (v, rem) = splitBy s " "
        in case trim rem of
            "" -> [Var v]
            _ -> Var v : readExpr rem

readLam :: String -> Expr
readLam s = case s of
    ('\\':rem) ->
        let (args, body) = splitBy rem "->"
        in createLam (words args) (readApps (trim body))
    _ -> error "readLam: строка не начинается с '\\'"

createLam :: [Symb] -> Expr -> Expr
createLam xs expr = foldr Lam expr xs

getParens :: String -> (String, String)
getParens ('(':rem) = helper 1 "" rem
    where
        helper :: Int -> String -> String -> (String, String)
        helper d s1 "" = (s1, "")
        helper d s1 (s:s2)
            | d + l == 0 = (s1, s2)
            | otherwise = helper (d + l) (s1++[s]) s2
            where
                l = case s of
                    '(' -> 1
                    ')' -> -1
                    _ -> 0
getParens _ = error "getParens: строка не начинается с '('"

splitBy :: String -> String -> (String, String)
splitBy "" _ = ("", "")
splitBy str@(x:xs) splitter
    | splitter `isPrefixOf` str = ("", drop (length splitter) str)
    | otherwise =
        let (s1, s2) = splitBy xs splitter
        in (x:s1, s2)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- TASK 1
freeVars :: Expr -> [Symb]
freeVars (Var s) = [s]
freeVars (t1 :@ t2) = freeVars t1 `union` freeVars t2
freeVars (Lam s expr) = filter (/= s) (freeVars expr)

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

-- TASK 4

nf :: Expr -> Expr
nf expr = maybe expr nf (reduceOnce expr)

-- TASK 5

infix 1 `betaEq`

betaEq :: Expr -> Expr -> Bool
betaEq f1 f2 = nf f1 `alphaEq` nf f2
