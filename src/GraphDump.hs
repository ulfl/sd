-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GraphDump where

import System.IO (Handle)
import Text.Heredoc
import Text.Printf (printf, hPrintf)

import Types

-- Dump nodes and edges of a Graph to GML format.
dumpGml :: Graph -> Graph -> Handle -> IO ()
dumpGml fullGraph graph handle = do
    edges <- dumpNodes handle fullGraph graph
    mapM_ (dumpEdge handle fullGraph) edges

dumpNodes :: Handle -> Graph -> Graph -> IO [Edge]
dumpNodes handle fullGraph graph = do
    case graph of
        Box name (Just (Annotation _path nodeId parentId)) -> do
            let nodeStr =
                    [str|node [
                        |   id %s
                        |   label "%s"
                        |   graphics [
                        |     type "roundrectangle"
                        |     fill "#ffcc00"
                        |     outline "#000000"
                        |     w %s.0
                        |     h 30.0
                        |   ]
                        |   gid %s
                        | ]
                        |]
            hPrintf
                handle
                nodeStr
                (show nodeId)
                (show name)
                (nodeWidth name)
                (show parentId)
            pure []
        Box _ Nothing -> internalError
        Group name style children edges (Just (Annotation _path nodeId parentId)) -> do
            let nodeStr =
                    [str|node [
                        |   id %s
                        |   graphics [
                        |     type "roundrectangle"
                        |     hasFill 0
                        |     outline "#000000"
                        |     outlineStyle "dashed"
                        |   ]
                        |   %s
                        |   isGroup 1
                        |   gid %s
                        | ]
                        |]
                labelStr =
                    [str| label "%s"
                        | LabelGraphics
                        |  [
                        |    text "%s"
                        |    fill "#EBEBEB"
                        |    fontSize 15
                        |    fontName	"Dialog"
                        |    alignment "left"
                        |    autoSizePolicy "node_width"
                        |    anchor "t"
                        |    borderDistance 0.0
                        |  ]
                        |]
                label =
                    case style of
                        Default ->
                            (printf labelStr (show name) (show name)) :: String
                        InvisibleLabel -> ""
            hPrintf
                handle
                nodeStr
                (show nodeId)
                label
                (show parentId)
            edges' <- mapM (dumpNodes handle fullGraph) children
            pure $ (edges ++ (concat edges'))
        Group _ _ _ _ Nothing -> internalError
  where
    nodeWidth :: NodeName -> String
    nodeWidth name =
        case (length $ show name) < 4 of
            True -> "30"
            False -> printf "%d" ((length $ show name) * 9)

dumpEdge :: Handle -> Graph -> Edge -> IO ()
dumpEdge handle fullGraph (Arrow n1 n2) = do
    let edgeStr =
            [str|edge
                |[
                |   source %s
                |   target %s
                |   graphics
                |   [
                |     fill "#000000"
                |     targetArrow "standard"
                |   ]
                | ]
                |]
    hPrintf
        handle
        edgeStr
        (show (lookupNode n1 fullGraph))
        (show (lookupNode n2 fullGraph))
  where
    lookupNode :: [NodeName] -> Graph -> NodeId
    lookupNode path graph =
        case lookupNode' path graph of
            [nodeId] -> nodeId
            _ -> error (printf "Name not found %s" $ show path)
    lookupNode' :: [NodeName] -> Graph -> [NodeId]
    lookupNode' path graph =
        case graph of
            Box _name (Just (Annotation path' nodeId _parentId)) ->
                case path' == path of
                    True -> [nodeId]
                    False -> []
            Box _ Nothing -> internalError
            Group _name _style children _edges (Just (Annotation path' nodeId _parentId)) ->
                case path' == path of
                    True -> [nodeId]
                    False -> concat $ map (lookupNode' path) children
            Group _ _ _ _ Nothing -> internalError

internalError :: a
internalError = error "Internal error."

header :: String
header =
    [str|Creator "hgml"
        |graph
        |[
        |  hierarchic 1
        |  name "some name"
        |  directed 1
        |]

footer :: String
footer = "]"
