{-# LANGUAGE OverloadedStrings #-}

module Adserver where

import Types
import Utils

main :: Graph
main = gGroup "My System" [euSetup] []

euSetup :: Graph
euSetup =
    gGroup
        "EU"
        [adserverEu, gBox "Customers"]
        [gArrow ["Customers"] ["aws-eu-west-1", "Ad Server", "ELB"]]

adserverEu = adserver ["aws-eu-west-1", "Ad Server"] app db log
  where
    app = ["1a", "1b", "1c"]
    db = take 5 $ cycle app
    log = ["1c"]

adserver :: [NodeName] -> [NodeName] -> [NodeName] -> [NodeName] -> Graph
adserver name appZones dbZones logZones = gGroupNested name nodes arrows
  where
    nodes = appNodes ++ dbNodes ++ logNodes ++ [gBox "ELB"]
    appNodes = nodesInGroups "app" appZones
    dbNodes = nodesInGroups "db" dbZones
    logNodes = nodesInGroups "log" logZones
    arrows =
        (map (\app -> gArrow ["ELB"] app) (pathsOfNodesInGroup appNodes) ++
         zipWith
             (\app db -> gArrow app db)
             (pathsOfNodesInGroup appNodes)
             (pathsOfNodesInGroup dbNodes))
