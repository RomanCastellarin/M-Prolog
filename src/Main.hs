module Main where

import AST
import Parser
import Text.ParserCombinators.Parsec    (parse)


printrows [] = return ()
printrows (x:xs) = print x >> printrows xs

main :: IO ()
main = do source <- readFile "test.pl"
          case parse (blanks *> rules) "main" source of
            Left  e -> print e >> fail "parse error"
            Right r -> printrows r >> return ()
          putStrLn "Done!"
