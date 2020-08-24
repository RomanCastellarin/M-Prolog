module Interpreter where

import AST
import Data.Maybe                       (fromMaybe, maybeToList, mapMaybe, fromJust)
import Control.Applicative              (liftA2)
import Control.Monad                    (foldM, MonadPlus, mzero)
import Text.Read                        (readMaybe)
import Data.List                        (intercalate)


-- ====================================================
--              CANDIDATE FOR PRODUCTION
-- ====================================================

-- renombra una regla para que todas las variables aparezcan nuevas
renameRule :: Rule -> Rule
renameRule r = case r of
    Rule i p xs -> Rule i (renamePred p) $ renamePred <$> xs
    where   renamePred p = case p of
                Predicate n x l -> Predicate n x $ renameTerm <$> l
                IsExpr x e      -> IsExpr (advance x) $ advance <$> e
                CompExpr c x e  -> CompExpr c (advance x) $ advance <$> e
            renameTerm t = case t of
                P p -> P $ renamePred p
                V v -> V (advance v)
                t -> t
            advance (Variable n s) = Variable (n+1) s

-- compone dos substituciones
-- t (compose s1 s2) = t (s1∘s2) = (t s1) s2
compose :: Unifier -> Unifier -> Unifier
compose σ1 σ2 = σ1' ++ σ2'
    where σ1' = [(v, t2) | (v, t) <- σ1, let t2 = fromJust $ substTerm σ2 t, V v /= t2]
          σ2' = [(v, t)  | (v, t) <- σ2, lookup v σ1 == Nothing]

-- SUBSTITUCIONES 

-- construye una expr. aritmética equivalente al término asociado a la variable 
replaceArith :: Unifier -> Variable -> Maybe ArithExp
replaceArith σ v = maybe (Just $ return v) convert $ lookup v σ
    where convert (A (Atom n)) = IntConst <$> readMaybe n
          convert (V w)        = Just (IntVar w)
          convert _            = Nothing

-- aplica un unificador al término
substTerm :: Unifier -> Term -> Maybe Term
substTerm σ t = case t of
        P p -> P <$> substPredicate σ p
        V v -> Just $ fromMaybe (V v) $ lookup v σ
        t   -> Just t

-- aplica un unificador a un predicado
substPredicate :: Unifier -> Predicate -> Maybe Predicate
substPredicate σ p = case p of
    Predicate d p l -> Predicate d p <$> mapM (substTerm σ) l
    IsExpr v e      -> IsExpr v <$> (e >>=? replaceArith σ)
    CompExpr c v e  -> CompExpr c v <$> (e >>=? replaceArith σ)

-- UNIFICATION

-- chequea si la variable aparece libre en el término
isIn :: Variable -> Term -> Bool
isIn v t = case t of
        P (Predicate _ _ p) -> (v `isIn`) `any` p
        t                   -> V v == t


-- unifica predicados
unifyPredicates :: Predicate -> Predicate -> Maybe Unifier
unifyPredicates p1 p2 = case (p1, p2) of
    (Predicate _ n1 t1, Predicate _ n2 t2) | n1 == n2 && length t1 == length t2 -> foldM unifyPair identity $ zip t1 t2
                                           | otherwise                          -> Nothing
    where unifyPair σ (t1, t2) = do t1' <- substTerm σ t1
                                    t2' <- substTerm σ t2
                                    compose σ <$> unifyTerms t1' t2'

-- unifica terminos
unifyTerms :: Term -> Term -> Maybe Unifier
unifyTerms t1 t2 = case (t1, t2) of
    (P p1, P p2)                         -> unifyPredicates p1 p2
    (V v, t)     | not (v `isIn` t)      -> return [(v, t)]
    (t, V v)     | not (v `isIn` t)      -> return [(v, t)]
    (t1, t2)     | t1 == t2              -> return identity
                 | otherwise             -> Nothing

-- muestra la cola de una lista
showListTail :: Term -> String
showListTail t = case t of
    A (Atom "nil")                  -> "]"
    P (Predicate _ "cons" [p, q])   -> ", " ++ showTerm p ++ showListTail q
    t                               -> " | " ++ showTerm t ++ "]" 

-- muestra un termino
showTerm :: Term -> String
showTerm t = case t of
    A a -> case a of Atom x | x ==  "nil" -> "[]" | otherwise -> x
    V v -> showVariable v
    P p -> showPredicate p

-- muestra una variable
showVariable :: Variable -> String
showVariable v = case v of
    Variable i v -> v -- ++ "_" ++ show i

-- muestra una expresion aritmetica
showArith :: ArithExp -> String
showArith e = case e of
    IntPlus x y  -> "(" ++ (showArith x) ++ "+" ++ (showArith y) ++ ")"
    IntMinus x y -> "(" ++ (showArith x) ++ "-" ++ (showArith y) ++ ")"
    IntTimes x y -> "(" ++ (showArith x) ++ "*" ++ (showArith y) ++ ")"
    IntDiv x y   -> "(" ++ (showArith x) ++ "/" ++ (showArith y) ++ ")"
    IntVar x     -> showVariable x
    IntConst x   -> show x

-- muestra una relacion de orden
showOrdering :: Ordering -> String
showOrdering o = case o of
    LT -> "<"
    GT -> ">"
    EQ -> "="

-- muestra un predicado
showPredicate :: Predicate -> String
showPredicate p = case p of
    Predicate _ "cons" [p, q]   -> "[" ++ showTerm p ++ showListTail q
    Predicate d name terms      -> (if d then "" else "~") ++ name ++ "(" ++ intercalate ", " (showTerm <$> terms) ++ ")"
    IsExpr v e                  -> showVariable v ++ " is " ++ showArith e
    CompExpr o v e              -> showVariable v ++ " " ++ showOrdering o ++ " " ++ showArith e

-- genera una solucion nueva para un predicado
-- (puede ser reemplazada en un futuro si la derivacion es exitosa)
generateSolution :: RuleID -> Predicate -> Unifier -> Solution
generateSolution i goal σ = (σ, Proof i ("can't derive " ++ (showPredicate . fromJust . substPredicate σ)  goal) [])

-- reescribe una derivacion exitosa
refreshSolution :: Predicate -> Solution -> Solution
refreshSolution p (σ, Proof i _ tree) = (σ, Proof i (writeProof p σ) tree)
    where writeProof p σ = showPredicate . fromJust $ substPredicate σ p

-- compone soluciones
composeSolutions :: Solution -> Solution -> Solution
composeSolutions (σ₁, Proof i s xs) (σ₂, p) = (compose σ₁ σ₂, Proof i s (xs ++ [p]))

-- evalua una expresion aritmetica y devuelve su resultado como valor monadico, o falla
evalArith :: (MonadPlus m) => ArithExp -> m Integer
evalArith a = case a of
    IntConst x   -> return x
    IntPlus  x y -> liftA2 (+) (evalArith x) (evalArith y)
    IntTimes x y -> liftA2 (*) (evalArith x) (evalArith y)
    IntMinus x y -> liftA2 (-) (evalArith x) (evalArith y)
    IntDiv   x y -> do a <- evalArith x
                       b <- evalArith y
                       if b /= 0 then return (a `div` b) else mzero
    _ -> mzero -- (IntVar) NOTE: should never occur for a proper program as variables should have been substituted



resolve :: Predicate -> Program -> [Solution]
resolve goal rules = mapMaybe matchRule (renameRule <$> rules) >>= backtrack
    where 
          matchRule rule@(Rule i lhs _) =  (,) rule <$> (generateSolution i goal <$> unifyPredicates goal lhs)

          backtrack (Rule i lhs clauses, sol@(σ, proof)) = refreshSolution lhs <$> foldM combine sol clauses --[(σ, proof)]

          combine (σ, proof) p = composeSolutions (σ, proof) <$> case p of
            IsExpr x e ->  e >>=? maybeToList . replaceArith σ >>= evalArith >>= isExprSolution σ x
            CompExpr c x e -> maybeToList (e >>=? replaceArith σ >>= evalArith >>= compExprSolution σ x c)
            Predicate True _ _ ->  predSolution σ p (renameRule <$> rules)
            Predicate False _ _ ->  if null $ predSolution σ p (renameRule <$> rules) then [(σ, proof)] else []

          isExprSolution σ x n = case lookup x σ of
                                    Just t  -> if t == t' then return (identity, Proof aritRule (isExprProof x n) []) else mzero
                                    Nothing -> return ([(x, t')], Proof aritRule (isExprProof x n) [])
                                where t' = A . Atom . show $ n

          isExprProof (Variable _ x) n = x ++ " is " ++ show n

          compExprProof (Variable _ x) c n = x ++ " " ++ showOrdering c ++ " " ++ show n

          compExprSolution σ x c n = do t <- lookup x σ
                                        m <- case t of
                                            A (Atom m) -> readMaybe m
                                            _          -> Nothing
                                        if c == compare m n
                                        then return (identity, Proof aritRule (compExprProof x c n) [])
                                        else Nothing

          predSolution σ p rs = do q <- maybeToList (substPredicate σ p)
                                   resolve q rs

