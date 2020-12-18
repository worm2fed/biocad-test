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
    { smiles         :: String
    , name           :: Maybe String
    , classification :: String
    } deriving (Show, Generic)
makeNodeLike ''Catalyst


catalysts :: [Catalyst]
catalysts =  map (\(a, b, c) -> Catalyst a b c)
    [ ( "[O-][Mn](=O)(=O)=O.[K+]"
      , Just "Potassium permanganate"
      , "Heterogeneous"
      )
    , ( "[Pt]"
      , Just "Platinum"
      , "Homogeneous"
      )
    , ( "[Pd]"
      , Just "Palladium"
      , "Homogeneous"
      )
    , ( "[Fe]"
      , Just "Iron"
      , "Homogeneous"
      )
    , ( "O=[V]=O"
      , Just "Vanadium dioxide"
      , "Heterogeneous"
      )
    , ( "C1CC(NC1)C(=O)O"
      , Just "Proline"
      , "Bio"
      )
    , ( "diastase"
      , Just "Diastase"
      , "Bio"
      )
    , ( "lactase"
      , Just "Lactase"
      , "Bio"
      )
    , ( "CC1=CC=C(C=C1)SC2=C(C=C(C=C2)C=C3C(=O)NC(=S)S3)C(F)(F)F"
      , Just "DNA polymerase"
      , "Bio"
      )
    , ( "alp"
      , Just "alkaline phosphatase (ALP)"
      , "Bio"
      )
    , ( "]+K[.O=)O=()O=(]nM[]-O["
      , Nothing
      , "Heterogeneous"
      )
    , ( "O)O=(C)1CN(CC1C"
      , Nothing
      , "Homogeneous"
      )
    , ( "C1CC"
      , Nothing
      , "Bio"
      )
    , ( "NC(=S)S3)C(F)(F)"
      , Nothing
      , "Bio"
      )
    , ( "C(C=C1)SC2=C"
      , Nothing
      , "Heterogeneous"
      )
    , ( "1CN"
      , Nothing
      , "Heterogeneous"
      )
    , ( "O=()O"
      , Nothing
      , "Homogeneous"
      )
    , ( "SC2=C(C=C(C=C2)C=C3C"
      , Nothing
      , "Bio"
      )
    , ( "(C3C=C)2C=C(C=C(C=2CS)"
      , Nothing
      , "Bio"
      )
    , ( "F)F()F(C)3S)S=(CN)O=(C3C=C)2C=C(C=C(C=2CS)1C=C(C=CC=1CC"
      , Nothing
      , "Heterogeneous"
      )
    ]
