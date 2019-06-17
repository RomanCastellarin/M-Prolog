module Interpreter where

import AST
import Data.Maybe           (fromMaybe) --maybeToList)
import Control.Monad        (foldM)
--import Text.Read            (readMaybe)


-- NOTE: Term could be a monad but I'd consider it really an abuse:
-- ~ substitute :: Unifier -> Variable -> Term
-- ~ substitute u v = fromMaybe (V v) (lookup v u)


replaceArith :: Unifier -> Variable -> ArithExp
replaceArith u v = maybe (IntVar v) convert $ lookup v u
    where convert (A (Atom n)) = (IntConst $ read n)
          convert (V w)        = IntVar w
          convert _            = IntVar v -- TODO: how to catch this error?

substPredicate :: Unifier -> Predicate -> Predicate
substPredicate u p = case p of
    Predicate n x l -> Predicate n x $ map (substitute u) l
    IsExpr x e      -> IsExpr x $ e >>= replaceArith u
    CompExpr c x e  -> CompExpr c x $ e >>= replaceArith u

substitute :: Unifier -> Term -> Term
substitute u t = case t of
        P p -> P $ substPredicate u p
        V v -> fromMaybe (V v) (lookup v u)
        t -> t

evalArith :: ArithExp -> [Integer]
evalArith a = case a of
    IntConst x   -> [x]
    IntPlus  x y -> (+) <$> evalArith x <*> evalArith y
    IntTimes x y -> (*) <$> evalArith x <*> evalArith y
    IntMinus x y -> (-) <$> evalArith x <*> evalArith y
    IntDiv   x y -> do a <- evalArith x
                       b <- evalArith y
                       if b /= 0 then [a `div` b] else []
    -- IntVar (Variable _ n) -> maybeToList $ readMaybe n -- should never occur !
    _ -> []


comp :: Unifier -> Unifier -> Unifier
comp u1 u2 = [(v, t2) | (v, t) <- u1, let t2 = substitute u2 t, (V v) /= t2] ++ [(v,t) | (v,t) <- u2, lookup v u1 == Nothing]

--{x/a, y/f(z), z/y} y τ={u/a, x/b, y/z, z/g(x)}
sigma1 = [(Variable 0 "X", A (Atom "a")),
          (Variable 0 "Y", P (Predicate True "f" [V (Variable 0 "Z")])),
          (Variable 0 "Z", V (Variable 0 "Y"))]

sigma2 = [(Variable 0 "U", A (Atom "a")),
          (Variable 0 "X", A (Atom "b")),
          (Variable 0 "Y", V (Variable 0 "Z")),
          (Variable 0 "Z", P (Predicate True "g" [V (Variable 0 "X")]))]

{--
[(Variable 0 "X",A (Atom "a")),
(Variable 0 "Y", P (Predicate True "f" [P (Predicate True "g" [V (Variable 0 "X")])])),
(Variable 0 "U", A (Atom "a"))]
--}

isIn :: Variable -> Term -> Bool
isIn v t = case t of
        P (Predicate _ _ p) -> (v `isIn`) `any` p
        t                   -> V v == t

unify :: Term -> Term -> Maybe Unifier
unify t1 t2 = case (t1, t2) of
    (t1, t2)     | t1 == t2              -> return identity
    (P p1, P p2) | checkPredicates p1 p2 -> foldM unifyPair identity $ zip (getTerms p1) (getTerms p2)
    (V v, t)     | not (v `isIn` t)      -> return [(v, t)]
    (t, V v)     | not (v `isIn` t)      -> return [(v, t)]
    (_, _)                               -> Nothing
  where checkPredicates (Predicate n1 x1 l1) (Predicate n2 x2 l2) = n1 == n2 && x1 == x2 && length l1 == length l2
        unifyPair u (p1, p2) = unify (substitute u p1) (substitute u p2)
        getTerms (Predicate _ _ l) = l

