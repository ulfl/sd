-- Copyright (C) 2017 Ulf Leopold.
module Interpret where

import Data.Char (toUpper, toLower)
import Data.Typeable (Typeable)
import Language.Haskell.Interpreter
       (runInterpreter, loadModules, setImports, interpret, as,
        InterpreterError(..), GhcError(..))
import System.FilePath (takeBaseName)
import Text.Printf (printf)

import Types

interpretGraph :: FilePath -> IO (Graph, (Graph -> [NodeName] -> [[Tag]] -> [Style]))
interpretGraph filePath =
    interpretHaskell filePath (as :: (Graph, (Graph -> [NodeName] -> [[Tag]] -> [Style])))

interpretHaskell
    :: Typeable a
    => String -> a -> IO a
interpretHaskell filePath typeWitness = do
    let baseName = takeBaseName filePath
    res <-
        runInterpreter $ do
            loadModules [filePath]
            setImports ["Prelude", "Types", baseName]
            interpret (baseName ++ ".main") typeWitness
    case res of
        Left err -> do
            case err of
                WontCompile errors ->
                    mapM_ (\(GhcError {errMsg = e}) -> putStrLn e) errors
                e -> putStrLn $ show e
            error $ printf "Error interpreting file: %s." filePath
        Right x -> return x

capitalize :: String -> String
capitalize (x:xs) = toUpper x : map toLower xs
capitalize [] = []
