-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE OverloadedStrings #-}

module Graph where

import Types

-- Assign node numbers to all nodes in the graph and add annotation consisting
-- of node numbers and the full node path.
annotateGraph :: ([NodeName] -> [GroupStyle]) -> Graph -> Graph
annotateGraph styling graph =
    let (_, graph') = annotateGraph' graph [] (NodeId 1) (NodeId 0) styling
    in graph'

annotateGraph' ::  Graph -> [NodeName] -> NodeId -> NodeId -> ([NodeName] -> [GroupStyle]) -> (NodeId, Graph)
annotateGraph' graph basePath nodeId parentId styling =
    case graph of
        Node name dataRetention _ ->
            let nodeId' = inc nodeId
            in ( nodeId'
               , Node
                     name
                     dataRetention
                     (Just $
                      Annotation (appendToPath basePath [name]) nodeId' parentId []))
        Group name children edges _ ->
            let nodeId' = inc nodeId
                path' = appendToPath basePath [name]
                (nodeId'', annotatedChildren) =
                    annotateChildren children path' nodeId' nodeId' styling
            in ( nodeId''
               , Group
                     name
                     annotatedChildren
                     (scopeEdges path' edges)
                     (Just (Annotation path' nodeId' parentId (styling path'))))
        Level _ child -> annotateGraph' child basePath nodeId parentId styling 
        Empty -> (nodeId, Empty)
  where
    inc :: NodeId -> NodeId
    inc (NodeId x) = NodeId (x + 1)
    appendToPath :: [NodeName] -> [NodeName] -> [NodeName]
    appendToPath path name = path ++ name
    annotateChildren :: [Graph]
                     -> [NodeName]
                     -> NodeId
                     -> NodeId
                     -> ([NodeName] -> [GroupStyle])
                     -> (NodeId, [Graph])
    annotateChildren [] _cPath cNodeId _cParentId styling = (cNodeId, [])
    annotateChildren (c:children) cPath cNodeId cParentId styling =
        let (nodeId', ac) = annotateGraph' c cPath cNodeId cParentId styling
            (nodeId'', acs) = annotateChildren children cPath nodeId' cParentId styling
        in (nodeId'', ac : acs)
    scopeEdges :: [NodeName] -> [Edge] -> [Edge]
    scopeEdges _path [] = []
    scopeEdges path ((Arrow name1 name2):edges) =
        (Arrow (appendToPath path name1) (appendToPath path name2)) :
        scopeEdges path edges
