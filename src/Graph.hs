-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE OverloadedStrings #-}

module Graph where

import Types

-- Assign node numbers to all nodes in the graph and add annotation consisting
-- of node numbers and the full node path.
annotateGraph :: Graph -> Graph
annotateGraph graph =
    let (_, graph') = annotateGraph' graph [] (NodeId 1) (NodeId 0)
    in graph'

annotateGraph' :: Graph -> [NodeName] -> NodeId -> NodeId -> (NodeId, Graph)
annotateGraph' graph basePath nodeId parentId =
    case graph of
        Box name _ ->
            let nodeId' = inc nodeId
            in ( nodeId'
               , Box
                     name
                     (Just $
                      Annotation (appendToPath basePath [name]) nodeId' parentId))
        Group name style children edges _ ->
            let nodeId' = inc nodeId
                path' = appendToPath basePath [name]
                (nodeId'', annotatedChildren) =
                    annotateChildren children path' nodeId' nodeId'
            in ( nodeId''
               , Group
                     name
                     style
                     annotatedChildren
                     (scopeEdges path' edges)
                     (Just (Annotation path' nodeId' parentId)))
        Level _ child -> annotateGraph' child basePath nodeId parentId
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
                     -> (NodeId, [Graph])
    annotateChildren [] _cPath cNodeId _cParentId = (cNodeId, [])
    annotateChildren (c:children) cPath cNodeId cParentId =
        let (nodeId', ac) = annotateGraph' c cPath cNodeId cParentId
            (nodeId'', acs) = annotateChildren children cPath nodeId' cParentId
        in (nodeId'', ac : acs)
    scopeEdges :: [NodeName] -> [Edge] -> [Edge]
    scopeEdges _path [] = []
    scopeEdges path ((Arrow name1 name2):edges) =
        (Arrow (appendToPath path name1) (appendToPath path name2)) :
        scopeEdges path edges
