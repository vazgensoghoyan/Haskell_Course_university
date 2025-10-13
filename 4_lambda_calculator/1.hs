import Data.List (union)
import Text.Read ( Read(readPrec) )

type Symb = String 

infixl 2 :@

data Expr = Var Symb | Expr :@ Expr | Lam Symb Expr
    deriving Eq

-- TASK 6

instance Show Expr where
    showsPrec _ (Var symb) = showString symb
    showsPrec d (expr1 :@ expr2) =
        showParen (d > 2) $
            showsPrec 2 expr1 . showString " " . showsPrec 2 expr2
    showsPrec d (Lam symb expr) =
        showParen (d > 1) $
            showString "\\" . showString symb . showString " -> " . showsPrec 1 expr

instance Read Expr where
    readsPrec _ s =
        case parseExpr (dropWhile (==' ') s) of
            Just (e, rest) -> [(e, rest)]
            Nothing        -> []

-- Парсим выражение
parseExpr :: String -> Maybe (Expr, String)
parseExpr s
    | null s = Nothing
    | head s == '\\' = parseLam s
    | head s == '('  = parseParens s
    | otherwise      = parseAppOrVar s

-- Лямбда с одним или несколькими аргументами: \x1 x2 -> body
parseLam :: String -> Maybe (Expr, String)
parseLam ('\\':rest) = do
    let (args, rest1) = spanArgs (dropWhile (==' ') rest)
    case stripPrefix "->" rest1 of
        Just rest2 -> do
            (body, rest3) <- parseExpr rest2
            return (foldr Lam body args, rest3)
        Nothing -> Nothing
parseLam _ = Nothing

parseAppOrVar :: String -> Maybe (Expr, String)
parseAppOrVar s = do
    let (terms, rest) = parseTerms s
    case terms of
        []  -> Nothing
        [t] -> Just (t, rest)
        ts  -> Just (foldl1 (:@) ts, rest)

parseParens :: String -> Maybe (Expr, String)
parseParens ('(':rest) = do
    (e, rest1) <- parseExpr rest
    rest2 <- stripPrefix ")" rest1
    return (e, rest2)
parseParens _ = Nothing

parseTerms :: String -> ([Expr], String)
parseTerms s = go (dropWhile (==' ') s) []
  where
    go "" acc = (reverse acc, "")
    go str acc
        | head str == '(' =
            case parseParens str of
                Just (e, rest) -> go (dropWhile (==' ') rest) (e:acc)
                Nothing -> (reverse acc, str)
        | head str == '\\' =
            case parseLam str of
                Just (e, rest) -> go (dropWhile (==' ') rest) (e:acc)
                Nothing -> (reverse acc, str)
        | otherwise =
            let (v, rest) = span isVarChar str
            in if null v then (reverse acc, str)
               else go (dropWhile (==' ') rest) (Var v:acc)

spanArgs :: String -> ([Symb], String)
spanArgs s = go (dropWhile (==' ') s) []
  where
    go "" acc = (reverse acc, "")
    go str acc
        | "->" `isPrefixOf` str = (reverse acc, str)
        | otherwise =
            let (v, rest) = span isVarChar str
            in if null v then (reverse acc, str)
               else go (dropWhile (==' ') rest) (v:acc)

isVarChar :: Char -> Bool
isVarChar c = c `elem` ('_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

stripPrefix :: String -> String -> Maybe String
stripPrefix [] xs         = Just xs
stripPrefix _ []          = Nothing
stripPrefix (p:ps) (x:xs)
    | p == x              = stripPrefix ps xs
    | otherwise           = Nothing

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False
isPrefixOf (a:as) (b:bs) = a==b && isPrefixOf as bs

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
