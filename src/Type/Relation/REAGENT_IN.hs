{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Type.Relation.REAGENT_IN where

import           Database.Bolt.Extras.Template (makeURelationLike)
import           GHC.Generics                  (Generic)



data REAGENT_IN = REAGENT_IN
    deriving (Show, Generic)
makeURelationLike ''REAGENT_IN
