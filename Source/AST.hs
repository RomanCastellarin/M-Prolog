{-# LANGUAGE DeriveFunctor #-}

module Source.AST where

import Control.Applicative
import Control.Monad            (ap)
import Data.Functor.Identity

-- Árbol de Prueba
data Proof = Proof RuleID String [Proof]
    deriving Show

-- Reglas
data Rule = Rule RuleID Predicate [Predicate]
    deriving (Show)
    
-- Programa
type Program = [Rule]

-- Predicado
data Predicate = Predicate Direct String [Term]     -- TODO: AVOID P(<is/comp_expression>) SOLVED? (sol: bare predicate) check for similar situations
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

-- Expresiones Aritméticas
data IntExp a = IntConst   Integer
              | IntPlus    (IntExp a) (IntExp a)
              | IntMinus   (IntExp a) (IntExp a)
              | IntTimes   (IntExp a) (IntExp a)
              | IntDiv     (IntExp a) (IntExp a)
              | IntVar     a
    deriving (Show, Functor, Eq)

-- Expresiones Aritméticas sobre Variables
type ArithExp = IntExp Variable

-- Substitución: unificador más general 
type Unifier = [(Variable, Term)]

-- Solución: MGU + demostración
type Solution = (Unifier, Proof)

-- CONSTANTES

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
    t >>= f = runIdentity (t >>=? Identity . f)

infixl 1 >>=?
t >>=? f = case t of
    IntPlus x y  -> liftA2 IntPlus  (x>>=?f) (y>>=?f)
    IntMinus x y -> liftA2 IntMinus (x>>=?f) (y>>=?f)
    IntTimes x y -> liftA2 IntTimes (x>>=?f) (y>>=?f)
    IntDiv x y   -> liftA2 IntDiv   (x>>=?f) (y>>=?f)
    IntVar x     -> f x
    IntConst x   -> pure (IntConst x)
