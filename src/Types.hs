-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.String (IsString)
import Text.Printf (printf)

newtype NodeId =
    NodeId Integer

instance Show NodeId where
    show (NodeId nodeId) = show nodeId

newtype NodeName =
    NodeName String
    deriving (Eq, Ord, IsString)

newtype Tag =
    Tag String
    deriving (Eq, Ord, IsString)

instance Show NodeName where
    show (NodeName name) = printf "%s" name

instance Show Tag where
    show (Tag name) = printf "%s" name

data DataRetention
    = Stateless
    | Days
    | Months
    | Years
    deriving (Eq, Show)

data Style
    = ShowLabel Bool
    | LabelBackgroundColor String
    | BackgroundColor String
    deriving (Eq, Show)

data Edge =
    Arrow [NodeName]
          [NodeName]
    deriving (Show)

data Annotation =
    Annotation [NodeName]
               NodeId
               NodeId
               [Style]
    deriving (Show)

data Graph
    = Node NodeName
           DataRetention
           (Maybe Annotation)
    | Group NodeName
            [Tag]
            [Graph]
            [Edge]
            (Maybe Annotation)
    | Level NodeName
            Graph
    | Empty
    deriving (Show)

gNodeStateless :: NodeName -> Graph
gNodeStateless name = Node name Stateless Nothing

gNode :: NodeName -> DataRetention -> Graph
gNode name dataRetention = Node name dataRetention Nothing

gActor = gNodeStateless

gBox = gNodeStateless

gGroup :: NodeName -> [Graph] -> [Edge] -> Graph
gGroup name children edges = gGroupTagged name [] children edges

gGroupTagged :: NodeName -> [Tag] -> [Graph] -> [Edge] -> Graph
gGroupTagged name tags children edges = Group name tags children edges Nothing

-- Recursively nest a set of graphs inside groups with the provided names.
gGroupNested :: [NodeName] -> [Graph] -> [Edge] -> Graph
gGroupNested names children edges = gGroupNestedTagged names [] children edges

gGroupNestedTagged :: [NodeName] -> [Tag] -> [Graph] -> [Edge] -> Graph
gGroupNestedTagged names tags children edges =
    foldr
        (\name acc -> Group name [] [acc] [] Nothing)
        (Group (last names) tags children edges Nothing)
        (init names)

gGroupNestedWithLevel :: [NodeName] -> [Tag] -> NodeName -> [Graph] -> [Edge] -> Graph
gGroupNestedWithLevel names tags level children edges =
    foldr
        (\name acc -> Group name [] [acc] [] Nothing)
        (Group (last names) tags (map (gLevel level) children) edges Nothing)
        (init names)

gLevel :: NodeName -> Graph -> Graph
gLevel name child = Level name child

gArrow :: [NodeName] -> [NodeName] -> Edge
gArrow node1 node2 = Arrow node1 node2
