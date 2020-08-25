module Main (main) where

{-
    ============================================
                M-Prolog command line    
    ============================================
-}

-- MODULES                              imports
import Source.AST
import Source.Parser                    (predicate, rule, program, renameRules)
import Source.Interpreter               
import Source.Printer                   (showResults, showRule)

-- BASE LIBRARIES                       imports
import Data.List                        (intercalate)

import Control.Monad                    (when)
import Data.Maybe                       (fromMaybe)
import Text.Read                        (readMaybe)

-- 3rd PARTY LIBRARIES                  imports
import Text.ParserCombinators.Parsec    (parse)
import System.Directory                 (doesFileExist)


-- main
main :: IO ()
main = prompt Nothing

-- interfaz de usuario
prompt :: Maybe Program  -> IO()
prompt program = putStr "?- " >> getLine >>= processCommand
    where processCommand cmd = case cmd of
            "quit!"    -> return ()
            "show!"    -> (putStr . unlines)  (showRule <$> fromMaybe [] program) >> prompt program
            "load!"    -> loadProgram (prompt program) 
            "add!"     -> addRule program (prompt program)
            "remove!"  -> removeRule program (prompt program)
            query      -> executeQuery program query >> prompt program

-- pide ingresar una regla por IO y la adiciona al programa 
addRule :: Maybe Program -> IO() -> IO()
addRule program action = do putStr "Enter rule: "
                            input <- getLine
                            let prog = fromMaybe [] program 
                            case parse rule "<rule>" input of
                                Left err -> putStrLn ("Error: " ++ show err) >> action
                                Right r  -> prompt . return . renameRules $ (r:prog)

-- pide indicar una regla por IO y la elimina del programa 
removeRule :: Maybe Program -> IO() -> IO()
removeRule program action = do putStr "Enter rule number: "
                               input <- getLine
                               let prog = fromMaybe [] program
                               case readMaybe input >>= check prog of
                                 Nothing -> putStrLn "Error: invalid number" >> action
                                 Just n  -> prompt . return . renameRules $ delete prog n
    where check prog n = if 1 <= n && n <= length prog then return n else Nothing
          delete prog n = let (l,r) = splitAt (n-1) prog in l ++ tail r 
                            
-- toma un predicado y muestra su resolucion por pantalla
executeQuery :: Maybe Program -> String -> IO()
executeQuery program query = case result of
        Left  err  -> putStrLn ("Error: " ++ show err)
        Right pred -> less . showResults $ maybe [] (resolve pred) program
    where result = parse predicate "<query>" query
          less s = case s of
            []      -> return ()
            (s:ss)  -> putStr s >> getLine >>= \ i -> when (i == ";") $ less ss

-- pide ingresar un nombre de archivo y carga su contenido en memoria
loadProgram :: IO() -> IO()
loadProgram action = do putStr "Enter filename: "
                        filename <- getLine
                        exists <- doesFileExist filename
                        if not exists
                        then putStrLn ("Error: " ++ filename ++ " does not exist.") >> action
                        else do source <- readFile filename
                                case parse program filename source of
                                 Left  err     -> putStrLn ("Error: " ++ show err) >> action
                                 Right program -> prompt $ Just program
                                     
