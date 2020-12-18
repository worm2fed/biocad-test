{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Type.Relation.ACCELERATE where

import           Database.Bolt.Extras.Template (makeURelationLike)
import           GHC.Generics                  (Generic)


data ACCELERATE = ACCELERATE
    { temperature :: Float
    , pressure    :: Float
    } deriving (Show, Generic)
makeURelationLike ''ACCELERATE
