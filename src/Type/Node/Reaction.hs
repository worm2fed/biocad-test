{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}


module Type.Node.Reaction where

import           Type.Instances                ()

import           Database.Bolt.Extras.Template (makeNodeLike)
import           GHC.Generics                  (Generic)


data Reaction = Reaction
    { id   :: Int
    , name :: String
    } deriving (Show, Generic)
makeNodeLike ''Reaction


reactions :: [Reaction]
reactions = map (uncurry Reaction)
    [ (1, "RHEA:11672")
    , (2, "RHEA:10660")
    , (3, "RHEA:10008")
    , (4, "RHEA:10016")
    , (5, "RHEA:10040")
    , (6, "RHEA:48780")
    , (7, "RHEA:16505")
    , (8, "RHEA:37343")
    , (9, "RHEA:10028")
    , (10, "RHEA:10044")
    , (11, "RHEA:10048")
    , (12, "RHEA:10052")
    , (13, "RHEA:10064")
    , (14, "RHEA:10068")
    , (15, "RHEA:10084")
    , (16, "RHEA:10104")
    , (17, "RHEA:10112")
    , (18, "RHEA:10152")
    , (19, "RHEA:10156")
    , (20, "RHEA:10160")
    ]
