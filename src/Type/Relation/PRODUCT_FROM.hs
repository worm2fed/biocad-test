{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Type.Relation.PRODUCT_FROM where

import           Database.Bolt.Extras.Template (makeURelationLike)
import           GHC.Generics                  (Generic)


data PRODUCT_FROM = PRODUCT_FROM
    { amount :: Float
    } deriving (Show, Generic)
makeURelationLike ''PRODUCT_FROM
