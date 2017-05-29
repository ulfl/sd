-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE OverloadedStrings #-}
module GraphOps where

import Data.List (partition, sortBy)

import Types

-- In a top down manner, combine 'Group' nodes that are siblings and have the
-- same name into a single group with the given name.
combineGraph :: Graph -> Graph
combineGraph graph =
    case graph of
        Group name style children edges Nothing ->
            let (groups, other) =
                    partition
                        (\x ->
                             case x of
                                 Group _ _ _ _ Nothing -> True
                                 _ -> False)
                        children
            in Group
                   name
                   style
                   ((map combineGraph (meld groups)) ++ other)
                   edges
                   Nothing
        _ -> graph

meld :: [Graph] -> [Graph]
meld groups = meld' groups'
  where
    groups' =
        sortBy (\(Group n _ _ _ _) (Group n' _ _ _ _) -> compare n n') groups
    
meld' :: [Graph] -> [Graph]
meld' ((Group name style children edges Nothing):(Group name' style' children' edges' Nothing):rest) =
    case (name == name' && style == style') of
        True ->
            let combined =
                    (Group
                          name
                          style
                          (children ++ children')
                          (edges ++ edges')
                          Nothing)
            in meld (combined : rest)
        False ->
            (Group name style children edges Nothing) :
            meld' ((Group name' style' children' edges' Nothing) : rest)
meld' graphs = graphs

-- Remove all nodes below 'Level 'nodes with name 'NodeName'. Also remove all
-- 'Level' nodes themselves.
pruneBelowLevel :: NodeName -> Graph -> Graph
pruneBelowLevel level (Level level' graph)
    | level == level' = Empty
    | otherwise = graph
pruneBelowLevel level (Group name style children edges Nothing) =
    Group name style (map (pruneBelowLevel level) children) edges Nothing
pruneBelowLevel _level box@(Box _name Nothing) = box
pruneBelowLevel _level Empty = Empty
pruneBelowLevel _level _ = internalError

internalError :: a
internalError = error "Internal error."
