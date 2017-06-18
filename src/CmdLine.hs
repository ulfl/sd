-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module CmdLine
    ( cmd
    ) where

import System.Console.CmdArgs
import System.IO (Handle, stdout, withFile, IOMode(..), hPutStrLn)

import Graph
import GraphDump
import GraphOps
import Interpret
import Types

data Sd = Sd
    { inputFile :: String
    , outputFile :: String
    } deriving (Show, Data, Typeable)

cfg :: Sd
cfg =
    Sd
    { inputFile = def &= argPos 0 &= typ "INPUTFILE"
    , outputFile = def &= name "o" &= typFile &= help "Output file"
    } &=
    summary "SD, Copyright (C) 2017 Ulf Leopold"

cmd :: IO ()
cmd = do
    Sd {inputFile = inFile, outputFile = outFile} <- (cmdArgs cfg)
    (graph, styling) <- interpretGraph inFile
    let res' =
            ((annotateGraph styling) .
             combineGraph . (pruneBelowLevel "NodeDetails"))
                graph
    case outFile == "" of
        True -> dump stdout res'
        False -> withFile outFile WriteMode $ \handle -> do dump handle res'
  where
    dump :: Handle -> Graph -> IO ()
    dump handle graph = do
        hPutStrLn handle header
        dumpGml graph handle
        hPutStrLn handle footer
