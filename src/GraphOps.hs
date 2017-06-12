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
        Group name children edges Nothing ->
            let (groups, other) =
                    partition
                        (\x ->
                             case x of
                                 Group _ _ _ Nothing -> True
                                 _ -> False)
                        children
            in Group
                   name
                   ((map combineGraph (meld groups)) ++ other)
                   edges
                   Nothing
        _ -> graph

meld :: [Graph] -> [Graph]
meld groups = meld' groups'
  where
    groups' =
        sortBy (\(Group n _ _ _) (Group n' _ _ _) -> compare n n') groups
    
meld' :: [Graph] -> [Graph]
meld' ((Group name children edges Nothing):(Group name' children' edges' Nothing):rest) =
    case (name == name') of
        True ->
            let combined =
                    (Group
                          name
                          (children ++ children')
                          (edges ++ edges')
                          Nothing)
            in meld (combined : rest)
        False ->
            (Group name children edges Nothing) :
            meld' ((Group name' children' edges' Nothing) : rest)
meld' graphs = graphs

-- Remove all nodes below 'Level 'nodes with name 'NodeName'. Also remove all
-- 'Level' nodes themselves.
pruneBelowLevel :: NodeName -> Graph -> Graph
pruneBelowLevel level (Level level' graph)
    | level == level' = Empty
    | otherwise = graph
pruneBelowLevel level (Group name children edges Nothing) =
    Group name (map (pruneBelowLevel level) children) edges Nothing
pruneBelowLevel _level box@(Node _name _dataRetention Nothing) = box
pruneBelowLevel _level Empty = Empty
pruneBelowLevel _level _ = internalError

internalError :: a
internalError = error "Internal error."
