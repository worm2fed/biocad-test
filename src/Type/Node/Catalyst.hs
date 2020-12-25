{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}


module Type.Node.Catalyst where

import           Type.Instances                ()

import           Database.Bolt.Extras.Template (makeNodeLike)
import           GHC.Generics                  (Generic)


data CatalystType = Heterogeneous | Homogeneous | Bio
  deriving (Show, Generic)

data Catalyst = Catalyst
    { id             :: Int
    , smiles         :: String
    , name           :: Maybe String
    , classification :: String
    } deriving (Show, Generic)
makeNodeLike ''Catalyst


catalysts :: [Catalyst]
catalysts =  map (\(i, s, n, c) -> Catalyst i s n c)
    [ ( 1
      , "[O-][Mn](=O)(=O)=O.[K+]"
      , Just "Potassium permanganate"
      , "Heterogeneous"
      )
    , ( 2
      , "[Pt]"
      , Just "Platinum"
      , "Homogeneous"
      )
    , ( 3
      , "[Pd]"
      , Just "Palladium"
      , "Homogeneous"
      )
    , ( 4
      ,"[Fe]"
      , Just "Iron"
      , "Homogeneous"
      )
    , ( 5
      ,"O=[V]=O"
      , Just "Vanadium dioxide"
      , "Heterogeneous"
      )
    , ( 6
      ,"C1CC(NC1)C(=O)O"
      , Just "Proline"
      , "Bio"
      )
    , ( 7
      ,"diastase"
      , Just "Diastase"
      , "Bio"
      )
    , ( 8
      ,"lactase"
      , Just "Lactase"
      , "Bio"
      )
    , ( 9
      ,"CC1=CC=C(C=C1)SC2=C(C=C(C=C2)C=C3C(=O)NC(=S)S3)C(F)(F)F"
      , Just "DNA polymerase"
      , "Bio"
      )
    , ( 10
      ,"alp"
      , Just "alkaline phosphatase (ALP)"
      , "Bio"
      )
    , ( 11
      ,"]+K[.O=)O=()O=(]nM[]-O["
      , Nothing
      , "Heterogeneous"
      )
    , ( 12
      ,"O)O=(C)1CN(CC1C"
      , Nothing
      , "Homogeneous"
      )
    , ( 13
      ,"C1CC"
      , Nothing
      , "Bio"
      )
    , ( 14
      ,"NC(=S)S3)C(F)(F)"
      , Nothing
      , "Bio"
      )
    , ( 15
      ,"C(C=C1)SC2=C"
      , Nothing
      , "Heterogeneous"
      )
    , ( 16
      ,"1CN"
      , Nothing
      , "Heterogeneous"
      )
    , ( 17
      ,"O=()O"
      , Nothing
      , "Homogeneous"
      )
    , ( 18
      ,"SC2=C(C=C(C=C2)C=C3C"
      , Nothing
      , "Bio"
      )
    , ( 19
      ,"(C3C=C)2C=C(C=C(C=2CS)"
      , Nothing
      , "Bio"
      )
    , ( 20
      ,"F)F()F(C)3S)S=(CN)O=(C3C=C)2C=C(C=C(C=2CS)1C=C(C=CC=1CC"
      , Nothing
      , "Heterogeneous"
      )
    ]
