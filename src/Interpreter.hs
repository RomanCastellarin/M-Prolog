module Interpreter where

import AST
import Data.Maybe           (fromMaybe) --maybeToList)
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


isIn :: Variable -> Term -> Bool
isIn v t = case t of
        P (Predicate _ _ p) -> (v `isIn`) `any` p
        t                   -> V v == t


