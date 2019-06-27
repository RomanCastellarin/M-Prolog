{-# LANGUAGE DeriveFunctor #-}

module AST where

import Control.Applicative
import Control.Monad        (liftM, ap)


-- Árbol de Prueba
data Proof = Proof RuleID String [Proof]
    deriving Show

-- Reglas
data Rule = Rule RuleID Predicate [Predicate]
    deriving (Show)
    
-- Programa
type Program = [Rule]

-- Predicado
data Predicate = Predicate Direct String [Term]     -- TODO: AVOID P(<is/comp_expression>) SOLVED? check for similar situations
               | IsExpr Variable ArithExp
               | CompExpr Ordering Variable ArithExp
    deriving (Show, Eq)

-- Átomo
data Atom = Atom String
    deriving (Show, Eq)

-- Término
data Term = P Predicate
          | V Variable
          | A Atom
    deriving (Show, Eq)

-- Variable
data Variable = Variable Index String 
    deriving (Show, Eq)

-- Subíndice
type Index = Int

-- Identificador para Reglas
type RuleID = Int

-- ¿Cómputo directo del predicado o de su opuesto?
type Direct = Bool

-- Expresiones Aritméticas  -- TODO: should I abstract away the operation? (benefit: pattern matching)
data IntExp a = IntConst   Integer
             -- | IntNeg     (IntExp a)
              | IntPlus    (IntExp a) (IntExp a)
              | IntMinus   (IntExp a) (IntExp a)
              | IntTimes   (IntExp a) (IntExp a)
              | IntDiv     (IntExp a) (IntExp a)
              | IntVar  a
    deriving (Show, Functor, Eq)

-- Expresiones Aritméticas sobre Variables
type ArithExp = IntExp Variable

-- Substitución: unificador más general 
type Unifier = [(Variable, Term)]

-- Solución: MGU + demostración
type Solution = (Unifier, Proof)

-- Substitución neutral
identity :: Unifier
identity = []

-- Constructor de lista
cons :: String
cons = "cons"

-- Lista vacía
nil :: String
nil = "nil"

-- Regla Aritmética
aritRule :: RuleID
aritRule = -2

-- INSTANCIAS 

instance Applicative IntExp where
  pure  = return
  (<*>) = ap

instance Monad IntExp where
    return = IntVar
    t >>= f = case t of
        IntPlus x y  -> IntPlus  (x>>=f) (y>>=f)
        IntMinus x y -> IntMinus (x>>=f) (y>>=f)
        IntTimes x y -> IntTimes (x>>=f) (y>>=f)
        IntDiv x y   -> IntDiv   (x>>=f) (y>>=f)
        IntVar x     -> f x
