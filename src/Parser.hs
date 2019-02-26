module Parser where

import Control.Applicative              (liftA2)
import Text.ParserCombinators.Parsec
import Data.Functor                     (void)

import AST

empty_id :: RuleID
empty_id = -1

nonNewline :: Parser Char
nonNewline = noneOf "\n"

inbetween :: Char -> Char -> Parser a -> Parser a
inbetween a b = between (char a <* blanks) (char b <* blanks)

parens :: Parser a -> Parser a
parens = inbetween '(' ')'

charThen :: Char -> a -> Parser a
charThen c a = char c *> return a 

closePar :: Parser Char
closePar = char ')' <* blanks

validLetter :: Parser Char
validLetter =  alphaNum <|> char '_'

comment :: Parser ()
comment = void (char '%' *> manyTill nonNewline newline) <?> "comment"

blanks :: Parser ()
blanks = skipMany (comment <|> void space) <?> "whitespace"

quotation :: Parser String
quotation = char '\'' *> manyTill nonNewline (char '\'') <* blanks <?> "quotation"

identifier :: Parser String
identifier = liftA2 (:) lower (many validLetter) <* blanks <?> "identifier"    

varIndentifier :: Parser String
varIndentifier = liftA2 (:) (upper <|> char '_') (many validLetter) <* blanks <?> "variable name"

variable :: Parser Variable
variable = Variable 0 <$> varIndentifier <?> "variable"

integer :: Parser Integer
integer = liftA2 (*) (charThen '-' (-1) <|> return 1) (many1 digit <* blanks >>= return . read) <?> "integer"

atom :: Parser Atom
atom = Atom <$> (quotation <|> identifier <|> show <$> integer ) <?> "atom" 

barePredicate :: Parser Predicate
barePredicate = do direct <- (charThen '~' False) <|> return True
                   name <- identifier
                   xs <- parens $ sepBy1 term (char ',' <* blanks)
                   return $ Predicate direct name xs
                <?> "bare predicate"

arithExpression :: Parser ArithExp
arithExpression = (chainl1 arithTerm $ addop <* blanks) <?> "arithmetic expression"
    where arithTerm = chainl1 arithFact $ mulop <* blanks 
          arithFact = parens arithExpression <|> arithAtom
          arithAtom = (IntConst <$> integer) <|> (IntVar <$> variable) 
          mulop     = (charThen '*' IntTimes) <|> (charThen '/' IntDiv)
          addop     = (charThen '+' IntPlus) <|>  (charThen '-' IntMinus)

isExpression :: Parser Predicate
isExpression = IsExpr <$> variable <* string "is" <* blanks <*> arithExpression <?> "'is' expression"

compExpression :: Parser Predicate
compExpression = flip CompExpr <$> variable <*> comparator <* blanks <*> atom <?> "comparison expression"
    where comparator = (charThen '<' LT) <|> (charThen '>' GT) <|> (charThen '=' GT)
        
predicate :: Parser Predicate
predicate = try compExpression <|> isExpression <|> barePredicate <?> "predicate" 

bareTerm :: Parser Term
bareTerm = (V <$> variable) <|> (P <$> try barePredicate) <|> (A <$> atom) <?> "bare term"

term :: Parser Term
term = (V <$> variable) <|> (P <$> try predicate) <|> (A <$> atom) <?> "term"

rule ::  Parser Rule
rule = do hd <- barePredicate 
          tail <- (string ":-" *> blanks *> sepBy predicate (char ',' <* blanks) <|> return []) <* char '.' <* blanks
          return $ Rule empty_id hd tail
         <?> "rule"

renameRules :: [Rule] -> [Rule]
renameRules = map (\(i, Rule _ p t) -> Rule i p t) . zip [0..]

rules :: Parser [Rule]
rules = renameRules <$> many1 rule <?> "program"

-- TODO: list, should allow for [x1,x2,...,xn] and [x1,...,xk | Y ]
-- list :: Parser Predicate
-- list = do terms <- char '[' *> blanks *> sepBy1 bareTerm (char ',' <* blanks)
          
-- ~ list = inbetween '[' ']' (sepBy1 bareTerm (char ',' <* blanks)) >>= return . foldr cons (A $ Atom "nil") <?> "list"
    -- ~ where cons a b = P $ Predicate True "cons" [a, b]
