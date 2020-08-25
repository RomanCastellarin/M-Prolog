module Source.Parser where

{-
    ============================================
        Parsers for datatypes defined in AST    
    ============================================
-}

-- MODULES
import Source.AST

-- BASE LIBRARIES                       imports
import Control.Applicative              (liftA2)
import Data.Functor                     (void)

-- 3rd PARTY LIBRARIES
import Text.ParserCombinators.Parsec


nonNewline :: Parser Char
nonNewline = noneOf "\n"

inbetween :: Char -> Char -> Parser a -> Parser a
inbetween a b = between (char a <* blanks) (char b <* blanks)

parens :: Parser a -> Parser a
parens = inbetween '(' ')'

charThen :: Char -> a -> Parser a
charThen c a = char c *> return a

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
                   xs <- parens $ sepBy1 bareTerm (char ',' <* blanks)
                   return $ Predicate direct name xs
                <?> "bare predicate"

arithExpression :: Parser ArithExp
arithExpression = (chainl1 arithTerm $ addop <* blanks) <?> "arithmetic expression"
    where arithTerm = chainl1 arithFact $ mulop <* blanks 
          arithFact = parens arithExpression <|> arithAtom
          arithAtom = (IntConst <$> integer) <|> (IntVar <$> variable) 
          mulop     = (charThen '*' IntTimes) <|> (charThen '/' IntDiv)
          addop     = (charThen '+' IntPlus)  <|> (charThen '-' IntMinus)

isExpression :: Parser Predicate
isExpression = IsExpr <$> variable <* string "is" <* blanks <*> arithExpression <?> "'is' expression"

compExpression :: Parser Predicate
compExpression = flip CompExpr <$> variable <*> comparator <* blanks <*> arithExpression <?> "comparison expression"
    where comparator = (charThen '<' LT) <|> (charThen '>' GT) <|> (charThen '=' EQ)
        
predicate :: Parser Predicate
predicate = try compExpression <|> isExpression <|> barePredicate <?> "predicate" 

bareTerm :: Parser Term
bareTerm = list <|> (V <$> variable) <|> (P <$> try barePredicate) <|> (A <$> atom) <?> "bare term"

term :: Parser Term
term = list <|> (V <$> variable) <|> (P <$> try predicate) <|> (A <$> atom) <?> "term"

rule ::  Parser Rule
rule = do hd <- blanks *> barePredicate 
          tail <- (string ":-" *> blanks *> sepBy predicate (char ',' <* blanks) <|> return []) <* char '.' <* blanks
          return $ Rule empty_id hd tail
         <?> "rule"

renameRules :: Program -> Program
renameRules = map (\(i, Rule _ p t) -> Rule i p t) . zip [1..]

program :: Parser Program
program = renameRules <$> many1 rule <?> "program"

list :: Parser Term
list = do terms <- char '[' *> blanks *> sepBy bareTerm (char ',' <* blanks)
          ending <- tailNil <|> char '|' <* blanks *> bareTerm <* char ']' <* blanks
          return $ foldr consList ending terms
        <?> "list"
    where tailNil = A <$> charThen ']' (Atom nil) <* blanks
          consList t1 t2 = P $ Predicate True cons [t1, t2]

