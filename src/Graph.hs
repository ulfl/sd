-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE OverloadedStrings #-}

module Graph where

import Types

-- Assign node numbers to all nodes in the graph and add annotation consisting
-- of node numbers and the full node path.
annotateGraph :: (Graph -> [NodeName] -> [[Tag]] -> [GroupStyle])
 -> Graph -> Graph
annotateGraph styling graph =
    let (_, graph') = annotateGraph' graph ([], []) (NodeId 1) (NodeId 0) styling
    in graph'

annotateGraph'
    :: Graph
    -> ([NodeName], [[Tag]])
    -> NodeId
    -> NodeId
    -> (Graph -> [NodeName] -> [[Tag]] -> [GroupStyle])

    -> (NodeId, Graph)
annotateGraph' graph (namePath, tagPath) nodeId parentId styling =
    case graph of
        Node name dataRetention _ ->
            let nodeId' = inc nodeId
                path' = appendToPath namePath name
            in ( nodeId'
               , Node
                     name
                     dataRetention
                     (Just $
                      Annotation
                          (appendToPath namePath name)
                          nodeId'
                          parentId
                          (styling graph path' tagPath)))
        Group name tags children edges _ ->
            let nodeId' = inc nodeId
                path' = appendToPath namePath name
                tagPath' = appendToPath tagPath tags
                (nodeId'', annotatedChildren) =
                    annotateChildren children (path', tagPath') nodeId' nodeId' styling
            in ( nodeId''
               , Group
                     name
                     tags
                     annotatedChildren
                     (scopeEdges path' edges)
                     (Just (Annotation path' nodeId' parentId (styling graph path' tagPath'))))
        Level _ child -> annotateGraph' child (namePath, tagPath) nodeId parentId styling
        Empty -> (nodeId, Empty)
  where
    inc :: NodeId -> NodeId
    inc (NodeId x) = NodeId (x + 1)
    appendToPath :: [a] -> a -> [a]
    appendToPath path name = path ++ [name]
    appendNamePaths :: [NodeName] -> [NodeName] -> [NodeName]
    appendNamePaths p1 p2 = p1 ++ p2
    annotateChildren
        :: [Graph]
        -> ([NodeName], [[Tag]])
        -> NodeId
        -> NodeId
        -> (Graph -> [NodeName] -> [[Tag]] -> [GroupStyle])

        -> (NodeId, [Graph])
    annotateChildren [] _cPath cNodeId _cParentId styling = (cNodeId, [])
    annotateChildren (c:children) cPath cNodeId cParentId styling =
        let (nodeId', ac) = annotateGraph' c cPath cNodeId cParentId styling
            (nodeId'', acs) =
                annotateChildren children cPath nodeId' cParentId styling
        in (nodeId'', ac : acs)
    scopeEdges :: [NodeName] -> [Edge] -> [Edge]
    scopeEdges _path [] = []
    scopeEdges path ((Arrow name1 name2):edges) =
        (Arrow (appendNamePaths path name1) (appendNamePaths path name2)) :
        scopeEdges path edges
