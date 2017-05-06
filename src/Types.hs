-- Copyright (C) 2017 Ulf Leopold.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.String (IsString)
import Text.Printf (printf)

newtype NodeId = NodeId Integer

instance Show NodeId where
    show (NodeId nodeId) = show nodeId
 
newtype NodeName =
    NodeName String
    deriving (Eq, Ord, IsString)

data Style
    = Default
    | InvisibleLabel
    deriving (Eq, Show)

instance Show NodeName where
    show (NodeName name) = printf "%s" name

data Edge =
    Arrow [NodeName]
          [NodeName]
    deriving (Show)

data Annotation = Annotation [NodeName] NodeId NodeId deriving (Show)

data Graph
    = Box NodeName
          (Maybe Annotation)
    | Group NodeName
            Style
            [Graph]
            [Edge]
            (Maybe Annotation)
    deriving (Show)

gBox :: NodeName -> Graph
gBox name = Box name Nothing

gGroup :: NodeName -> [Graph] -> [Edge] -> Graph
gGroup name children edges = Group name Default children edges Nothing

gGroupInvisibleLabel :: NodeName -> [Graph] -> [Edge] -> Graph
gGroupInvisibleLabel name children edges = Group name InvisibleLabel children edges Nothing

gGroupNested :: [NodeName] -> [Graph] -> [Edge] -> Graph
gGroupNested names children edges =
    foldr
        (\name acc -> Group name Default [acc] [] Nothing)
        (Group (last names) Default children edges Nothing)
        (init names)

gArrow :: [NodeName] -> [NodeName] -> Edge
gArrow node1 node2 = Arrow node1 node2
