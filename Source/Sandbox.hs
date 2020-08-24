{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Source.Sandbox where

import Source.AST
import Source.Parser
import Text.ParserCombinators.Parsec    (parse)
import Interpreter
import Data.Maybe                       (fromMaybe, maybeToList, mapMaybe, fromJust)
import           Debug.Trace         (trace)

-- AUXILIARES

debug :: Show a => a -> a
debug x = trace ("debug: " ++ show x ++ "\n") x

-- ===============================================



-- NOTE: Term could be a monad but I'd consider it really an abuse:
-- ~ substitute :: Unifier -> Variable -> Term
-- ~ substitute u v = fromMaybe (V v) (lookup v u)

-- ~ unify :: Predicate -> Predicate -> [Solution]
-- ~ unify p1 p2 = foldM comb identity $ zip p1 p2

--{x/a, y/f(z), z/y}yτ={u/a, x/b, y/z, z/g(x)}
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


-- ====================================================
--                      EXAMPLES
-- ====================================================

quickparse p s = case parse p "quickparse" s of
    Left  e -> error (show e)
    Right r -> r

x1 = Variable 1 "X"
x2 = Variable 2 "X"

s1 = [(x1, V x2)]
s2 = [(x2, A $ Atom "Hello")]
s3 = compose s1 s2
s4 = [(x1, V x2), (x2, V x1)]

p1 = Predicate True "P" [V x1, V x2]
p2 = quickparse barePredicate "f(g(X),X)"
p3 = quickparse barePredicate "f(Y,a)"
p4 = quickparse barePredicate "f(X,Y)"
p5 = quickparse barePredicate "f(b,Y)"

t1 = quickparse term "[X, 15, X, [2, 3]]"
t2 = quickparse term "cons(1, cons(2, cons(3, X)))"

isexp1 = quickparse isExpression "X is 2 * (Y+YS-4*Z) / Z"
isexp2 = quickparse isExpression "X is 2 * (Y+YS 4*Z) / Z"
isexp3 = quickparse isExpression "X is 2 * (Y+YS-4*(-17)) / (-17)"
isexp4 = fromJust $ substPredicate [(Variable 0 "Z", A(Atom "-17"))] isexp1


