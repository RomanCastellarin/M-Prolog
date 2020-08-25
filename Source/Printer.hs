module Source.Printer where

{-
    ============================================
            Pretty prints for datatypes    
    ============================================
-}

-- MODULES                              imports
import Source.AST

-- BASE LIBRARIES                       imports
import Data.List                        (intercalate)

-- muestra una regla y su indice
showRule :: Rule -> String
showRule r = case r of
    Rule i p clauses -> show i ++ ") "
                        ++ showPredicate p
                        ++ if null clauses then "" else " :- "
                        ++ intercalate ", " (showPredicate <$> clauses) ++ "."

-- muestra un arbol de derivacion
showProof :: Proof -> String
showProof p = showProof_ 1 p
    where showProof_ i p = ([1..i] >>= const "  ") ++ case p of
            Proof n s xs | n == aritRule -> "(arithmetic) " ++ s ++ "\n"
                         | otherwise     -> "Rule " ++ show n ++ ": " ++ s ++ "\n" ++ concat (showProof_ (i+1) <$> xs)

-- muestra una solucion
showResults :: [Solution] -> [String]
showResults sols = if null sols then ["No"] else showSolution <$> sols
    where showSolution (σ, p) = "Solution:\n\t" ++ showUnifier σ ++ "\nProof:\n" ++ showProof p
          showUnifier σ = if null σ then "Yes" else intercalate " " $ showAssignment <$> filter (\(Variable i _, _) -> i == 0) σ
          showAssignment (v, t) = showVariable v ++ " = " ++ showTerm t 

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
