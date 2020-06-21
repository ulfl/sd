-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GraphDump where

import System.IO (Handle)
import Text.Heredoc
import Text.Printf (hPrintf, printf)

import Types

-- Dump nodes and edges of a Graph to GML format.
dumpGml :: Graph -> Handle -> IO ()
dumpGml graph = dumpGml' graph graph

dumpGml' :: Graph -> Graph -> Handle -> IO ()
dumpGml' fullGraph graph handle = do
  edges <- dumpNodes handle fullGraph graph
  mapM_ (dumpEdge handle fullGraph) edges

dumpNodes :: Handle -> Graph -> Graph -> IO [Edge]
dumpNodes handle fullGraph graph =
  case graph of
    Node name dataRetention (Just (Annotation _path nodeId parentId style)) -> do
      let nodeStr =
            [str|node [
                        |   id %s
                        |   label "%s"
                        |   graphics [
                        |     type "roundrectangle"
                        |     fill "%s"
                        |     outline "#000000"
                        |     outlineWidth %s
                        |     w %s.0
                        |     h 75.0
                        |   ]
                        |   LabelGraphics
                        |   [
                        |     text	"%s"
                        |     fontSize	24
                        |     fontName	"Dialog"
                        |     anchor	"c"
                        |   ]
                        |   gid %s
                        | ]
                        |]
      hPrintf
        handle
        nodeStr
        (show nodeId)
        (show name)
        (backgroundColor style)
        (outlineWidth dataRetention)
        (nodeWidth name)
        (show name)
        (show parentId)
      pure []
    Node _ _dataRetention Nothing -> internalError
    Group name _tags children edges (Just (Annotation _path nodeId parentId style)) -> do
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
                        |    fill "%s"
                        |    fontSize 28
                        |    fontName	"Dialog"
                        |    alignment "left"
                        |    autoSizePolicy "node_width"
                        |    anchor "t"
                        |    borderDistance 0.0
                        |  ]
                        |]
          label =
            if showLabel style
              then printf labelStr (show name) (show name) (labelColor style) :: String
              else ""
      hPrintf handle nodeStr (show nodeId) label (show parentId)
      edges' <- mapM (dumpNodes handle fullGraph) children
      pure (edges ++ concat edges')
    Group _ _ _ _ Nothing -> internalError
    Level _ child -> dumpNodes handle fullGraph child
    Empty -> pure []
  where
    nodeWidth :: NodeName -> String
    nodeWidth name =
      if length (show name) <= 4
        then "75"
        else printf "%d" (length (show name) * 17)
    outlineWidth :: DataRetention -> String
    outlineWidth Stateless = "1"
    outlineWidth Days = "2"
    outlineWidth Months = "4"
    outlineWidth Years = "6"
    showLabel :: [Style] -> Bool
    showLabel [] = True
    showLabel ((ShowLabel False):_) = False
    showLabel (_:x) = showLabel x
    labelColor :: [Style] -> String
    labelColor [] = "#ebebeb"
    labelColor ((LabelBackgroundColor x):_) = x
    labelColor (_:x) = labelColor x
    backgroundColor :: [Style] -> String
    backgroundColor [] = "#ffcc00"
    backgroundColor ((BackgroundColor x):_) = x
    backgroundColor (_:x) = backgroundColor x

dumpEdge :: Handle -> Graph -> Edge -> IO ()
dumpEdge handle fullGraph (Arrow n1 n2) = do
  let edgeStr =
        [str|edge
                |[
                |   source %s
                |   target %s
                |   graphics
                |   [
                |     width 2
                |     fill "#000000"
                |     targetArrow "standard"
                |   ]
                | ]
                |]
  let res = do
        id1 <- lookupNode n1 fullGraph
        id2 <- lookupNode n2 fullGraph
        pure (id1, id2)
  case res of
    Right (id1, id2) -> hPrintf handle edgeStr (show id1) (show id2)
    Left msg ->
      error $
      printf
        ("Can't find (or can find more than one) endpoint %s " ++
         "on edge\n: (%s, %s)")
        msg
        (show n1)
        (show n2)
  where
    lookupNode :: [NodeName] -> Graph -> Either String NodeId
    lookupNode path graph =
      case lookupNode' path graph of
        [nodeId] -> Right nodeId
        (_:_) -> Left $ show path
        [] -> Left $ show path
    lookupNode' :: [NodeName] -> Graph -> [NodeId]
    lookupNode' path graph =
      case graph of
        Node _name _dataRetention (Just (Annotation path' nodeId _parentId _style)) ->
          if path' == path
            then [nodeId]
            else []
        Node _ _dataRetention Nothing -> internalError
        Group _name _tags children _edges (Just (Annotation path' nodeId _parentId _style)) ->
          if path' == path
            then [nodeId]
            else concatMap (lookupNode' path) children
        Group _ _ _ _ Nothing -> internalError
        Level _ child -> lookupNode' path child
        Empty -> []

internalError :: a
internalError = error "Internal error."

header :: String
header =
  [str|Creator "sd"
        |graph
        |[
        |  hierarchic 1
        |  name "some name"
        |  directed 1
        |]

footer :: String
footer = "]"
