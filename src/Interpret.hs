-- Copyright (C) 2017 Ulf Leopold.
module Interpret where

import Data.Char (toUpper, toLower)
import Data.Typeable.Internal (Typeable)
import Language.Haskell.Interpreter
       (runInterpreter, loadModules, setImports, interpret, as,
        InterpreterError(..), GhcError(..))
import System.FilePath (takeBaseName)
import Text.Printf (printf)

import Types

interpretGraph :: FilePath -> IO Graph
interpretGraph filePath = interpretHaskell filePath (as :: Graph)

interpretHaskell
    :: Typeable a
    => String -> a -> IO a
interpretHaskell filePath typeWitness = do
    let baseName = takeBaseName filePath
    let moduleName = capitalize baseName
    res <-
        runInterpreter $ do
            loadModules [filePath]
            setImports ["Prelude", "Types", moduleName]
            interpret (moduleName ++ ".main") typeWitness --(as :: Graph)
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
