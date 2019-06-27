module Interpreter where

import AST
import Data.Maybe           (fromMaybe, maybeToList)
import Control.Monad        (foldM)
import Text.Read            (readMaybe)


extractInt :: Term -> Maybe Integer
extractInt t = case t of
    A (Atom s) -> readMaybe s
    _          -> Nothing

-- SUBSTITUCIONES 

-- (uso con bind) construye una expr. aritmética equivalente al término asociado a la variable
-- TODO: this function should be able to fail (hence substitutions could fail)  
replaceArith :: Unifier -> Variable -> ArithExp
replaceArith u v = maybe (IntVar v) convert $ lookup v u
    where convert (A (Atom n)) = (IntConst $ read n)    -- TODO: this could also fail?
          convert (V w)        = IntVar w
          convert _            = IntVar v -- TODO: how to catch this error?

-- aplica un unificador a un predicado
substPredicate :: Unifier -> Predicate -> Predicate
substPredicate u p = case p of
    Predicate n x l -> Predicate n x $ map (substitute u) l
    IsExpr x e      -> IsExpr x $ e >>= replaceArith u
    CompExpr c x e  -> CompExpr c x $ e >>= replaceArith u

-- aplica el unificador al término
substitute :: Unifier -> Term -> Term
substitute u t = case t of
        P p -> P $ substPredicate u p
        V v -> fromMaybe (V v) (lookup v u)
        t   -> t

-- EVALUACIONES

-- evalúa una expresión aritmética, falla si no está totalmente determinada
-- NOTE: There's a natural transformation Maybe -> List, so I use list which simplifies further work
evalArith :: ArithExp -> [Integer]
evalArith a = case a of
    IntConst x   -> [x]
    IntPlus  x y -> (+) <$> evalArith x <*> evalArith y
    IntTimes x y -> (*) <$> evalArith x <*> evalArith y
    IntMinus x y -> (-) <$> evalArith x <*> evalArith y
    IntDiv   x y -> do a <- evalArith x
                       b <- evalArith y
                       if b /= 0 then [a `div` b] else []
    -- IntVar (Variable _ n) -> [] -- NOTE: should never occur as variables should have been substituted
    _ -> []

-- COMPOSICIONES

-- composes two unifications
comp :: Unifier -> Unifier -> Unifier
comp u2 u1 = [(v, t2) | (v, t) <- u1, let t2 = substitute u2 t, (V v) /= t2] ++ [(v,t) | (v,t) <- u2, lookup v u1 == Nothing]


-- chequea si la variable aparece libre en el término
isIn :: Variable -> Term -> Bool
isIn v t = case t of
        P (Predicate _ _ p) -> (v `isIn`) `any` p
        t                   -> V v == t

-- UNIFICACIONES

-- retorna (si hubiere) una unificación entre dos términos
unify :: Term -> Term -> Maybe Unifier
unify t1 t2 = case (t1, t2) of
    (t1, t2)     | t1 == t2              -> return identity
    (P p1, P p2) | checkPredicates p1 p2 -> foldM unifyPair identity $ zip (getTerms p1) (getTerms p2)
    (V v, t)     | not (v `isIn` t)      -> return [(v, t)]
    (t, V v)     | not (v `isIn` t)      -> return [(v, t)]
    _                                    -> Nothing
  where checkPredicates (Predicate n1 x1 l1) (Predicate n2 x2 l2) = n1 == n2 && x1 == x2 && length l1 == length l2
        unifyPair u (p1, p2) = comp u <$> unify (substitute u p1) (substitute u p2)
        getTerms (Predicate _ _ l) = l


-- compone dos soluciones
buildSol :: Solution -> Solution -> Solution
buildSol (u1, Proof n s xs) (u2, p) = (comp u1 u2, Proof n s (p:xs))


evalIsExpr :: Variable -> ArithExp -> [Solution]
evalIsExpr v e =  flip (,) (Proof aritRule "<IsExpression>" []) . return <$> (,) v <$> A . Atom . show <$> evalArith e

evalCompExpr :: Ordering -> Maybe Term -> ArithExp -> Bool
evalCompExpr ord v e = checkOrder (maybeToList $ v >>= extractInt) (evalArith e)  
    where checkOrder x y = or [ord == (compare n1 n2) | n1 <- x, n2 <- y]

propagate :: Solution -> Predicate -> [Solution]
propagate (s, p) t = case t of
    IsExpr v e          -> buildSol (s, p) <$> evalIsExpr v e
    CompExpr ord v e    -> if evalCompExpr ord (lookup v s) e then return (s, p) else []
    --t                   -> solve t (freshen program)

{--
solveRule :: Predicate -> Rule -> [Solution]
solveRule p r = case r of
    Rule i q xs -> 


solve :: Predicate -> Program -> [Solution]
solve goal program = program >>= solveRule goal
--}

