{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Type.Instances where

import           Data.Text            (pack, unpack)
import           Database.Bolt        (Value (T))
import           Database.Bolt.Extras (FromValue, ToValue, fromValue, toValue)


instance {-# OVERLAPS #-} ToValue String where
    toValue :: String -> Value
    toValue = T . pack

instance {-# OVERLAPS #-} FromValue String where
    fromValue :: Value -> String
    fromValue (T textV) = unpack textV
