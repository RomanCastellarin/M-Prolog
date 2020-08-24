
module Main where

import AST
import Parser
import Text.ParserCombinators.Parsec    (parse)
import Interpreter
import Sandbox

import Data.List                        (intercalate)
import System.Directory                 (doesFileExist)
import Control.Monad                    (when)
import Data.Maybe                       (fromMaybe)
import Text.Read                        (readMaybe)



main :: IO ()
main = prompt Nothing

prompt :: Maybe Program  -> IO()
prompt program = putStr "?- " >> getLine >>= processCommand
    where processCommand cmd = case cmd of
            "quit!"    -> return ()
            "show!"    -> (putStr . unlines)  (showRule <$> fromMaybe [] program) >> prompt program
            "load!"    -> loadProgram (prompt program) 
            "add!"     -> addRule program (prompt program)
            "remove!"  -> removeRule program (prompt program)
            query      -> executeQuery program query >> prompt program

addRule :: Maybe Program -> IO() -> IO()
addRule program action = do putStr "Enter rule: "
                            input <- getLine
                            let prog = fromMaybe [] program 
                            case parse rule "<rule>" input of
                                Left err -> putStrLn ("Error: " ++ show err) >> action
                                Right r  -> prompt . return . renameRules $ (r:prog)

removeRule :: Maybe Program -> IO() -> IO()
removeRule program action = do putStr "Enter rule number: "
                               input <- getLine
                               let prog = fromMaybe [] program
                               case readMaybe input >>= check prog of
                                 Nothing -> putStrLn "Error: invalid number" >> action
                                 Just n  -> prompt . return . renameRules $ delete prog n
    where check prog n = if 1 <= n && n <= length prog then return n else Nothing
          delete prog n = let (l,r) = splitAt (n-1) prog in l ++ tail r 
                            

executeQuery :: Maybe Program -> String -> IO()
executeQuery program query = case result of
        Left  err  -> putStrLn ("Error: " ++ show err)
        Right pred -> less . showResults $ maybe [] (resolve pred) program
    where result = parse predicate "<query>" query
          less s = case s of
            []      -> return ()
            (s:ss)  -> putStr s >> getLine >>= \ i -> when (i == ";") $ less ss

loadProgram :: IO() -> IO()
loadProgram action = do putStr "Enter filename: "
                        filename <- getLine
                        exists <- doesFileExist filename
                        if not exists
                        then putStrLn ("Error: " ++ filename ++ " does not exist.") >> action
                        else do source <- readFile filename
                                case parse rules filename source of
                                 Left  err     -> putStrLn ("Error: " ++ show err) >> action
                                 Right program -> prompt $ Just program
                                     

showProof :: Int -> Proof -> String
showProof i p = case p of
        Proof n s xs | n == 0        -> "\n" ++ concat (showProof (i+1) <$> xs) -- FOR USER QUERY
                     | n == -1       -> space i ++ "Rule: " ++ s ++ "\n" ++ concat (showProof (i+1) <$> xs) -- WHEN is it because of goal ?
                     | n == aritRule -> space i ++ "(arithmetic) " ++ s ++ "\n"
                     | otherwise     -> space i ++ "Rule " ++ show n ++ ": " ++ s ++ "\n" ++ concat (showProof (i+1) <$> xs)
    where space n = [1..n] >>= const "  "

showResults :: [Solution] -> [String]
showResults sols = if null sols then ["No"] else showSolution <$> sols
    where showSolution (σ, p) = "Solution:\n\t" ++ showUnifier σ ++ "\nProof:\n" ++ showProof 1 p
          showUnifier σ = if null σ then "Yes" else intercalate " " $ showAssignment <$> filter (\(Variable i _, _) -> i == 0) σ
          showAssignment (Variable _ v, t) = v ++ " = " ++ showTerm t 

showRule :: Rule -> String
showRule r = case r of
    Rule i p clauses -> show i ++ ") " ++ showPredicate p ++ (if null clauses then "" else " :- ") ++  intercalate ", " (showPredicate <$> clauses) ++ "."
