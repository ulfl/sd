{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.String
import Text.Printf

import Types

nodesInGroups :: NodeName -> [NodeName] -> [Graph]
nodesInGroups baseName groups =
    zipWith
        (\groupName index ->
             gGroup
                 groupName
                 [(gBox (fromString (printf "%s%d" (show baseName) index)))]
                 [])
        groups
        ([1 ..] :: [Integer])

pathsOfNodesInGroup :: [Graph] -> [[NodeName]]
pathsOfNodesInGroup groups = concat $ map groupToPath groups
  where
    groupToPath (Group name _ [Group name' InvisibleLabel boxes _ _] _ _) =
        map (\(Box name'' _) -> [name, name', name'']) boxes
    groupToPath (Group name _ boxes _ _) =
        map (\(Box name' _) -> [name, name']) boxes
