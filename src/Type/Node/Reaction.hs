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
    { name :: String
    } deriving (Show, Generic)
makeNodeLike ''Reaction


reactions :: [Reaction]
reactions = map Reaction
    [ "RHEA:11672"
    , "RHEA:10660"
    , "RHEA:10008"
    , "RHEA:10016"
    , "RHEA:10040"
    , "RHEA:48780"
    , "RHEA:16505"
    , "RHEA:37343"
    , "RHEA:10028"
    , "RHEA:10044"
    , "RHEA:10048"
    , "RHEA:10052"
    , "RHEA:10064"
    , "RHEA:10068"
    , "RHEA:10084"
    , "RHEA:10104"
    , "RHEA:10112"
    , "RHEA:10152"
    , "RHEA:10156"
    , "RHEA:10160"
    ]
