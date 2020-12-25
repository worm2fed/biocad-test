{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module API (runQueryDB, putCatalyst, putMolecule, putReaction, putReactionWithRelations, findReaction, findPath) where

import           Control.Exception          (bracket)
import           Control.Monad              (forM)
import           Control.Monad.State        (execState, modify)
import           Data.Default               (Default (def))
import           Data.Foldable              (traverse_)
import           Data.Function              ((&))
import           Data.Text                  (Text, pack)

import           Database.Bolt              (BoltActionT, BoltCfg, Path, Pipe,
                                             at, close, connect, password,
                                             props, queryP, run, user, (=:))
import           Database.Bolt.Extras       (NodeLike (..), URelationLike (..))
import           Database.Bolt.Extras.Graph (GetRequest, GraphGetRequest,
                                             GraphPutRequest,
                                             GraphQuery (makeRequest),
                                             PutNode (CreateN, MergeN),
                                             PutRelationship (MergeR),
                                             PutRequest, addNode, addRelation,
                                             allProps, defaultNodeReturn,
                                             emptyGraph, extractNode,
                                             withBoltId, withLabelQ, withReturn)

import qualified Type.Node.Catalyst         as C
import qualified Type.Node.Molecule         as M
import qualified Type.Node.Reaction         as R
import           Type.Relation.ACCELERATE   (ACCELERATE (..))
import           Type.Relation.PRODUCT_FROM (PRODUCT_FROM (..))
import           Type.Relation.REAGENT_IN   (REAGENT_IN (..))


-- | Configuration for connection to local database
neo4j :: BoltCfg
neo4j = def
    { user = "neo4j"
    , password = "s3cr3t"
    }

-- | Connection with neo4j
connection :: IO Pipe
connection = connect neo4j

-- | Helper to run queries in Neo4j DB
runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket connection close (`run` act)


-- | Builds query to create Catalyst
prepareCatalyst :: C.Catalyst -> GraphPutRequest
prepareCatalyst catalyst = flip execState emptyGraph $ do
        modify $ addNode "c" (CreateN . toNode $ catalyst)

-- | Put Catalyst to the database
putCatalyst :: C.Catalyst -> IO ()
putCatalyst catalyst = do
    request <- runQueryDB $ makeRequest @PutRequest []
                          $ prepareCatalyst catalyst
    print request


-- | Builds query to create Molecule
prepareMolecule :: M.Molecule -> GraphPutRequest
prepareMolecule molecule = flip execState emptyGraph $ do
        modify $ addNode "m" (CreateN . toNode $ molecule)

-- | Put Molecule to the database
putMolecule :: M.Molecule -> IO ()
putMolecule molecule = do
    request <- runQueryDB $ makeRequest @PutRequest []
                          $ prepareMolecule molecule
    print request


-- | Builds query to create Reaction
prepareReaction :: R.Reaction -> GraphPutRequest
prepareReaction reaction = flip execState emptyGraph $ do
        modify $ addNode "r" (CreateN . toNode $ reaction)

-- | Put Reaction to the database
putReaction :: R.Reaction -> IO ()
putReaction reaction = do
    request <- runQueryDB $ makeRequest @PutRequest []
                          $ prepareReaction reaction
    print request


-- | Builds query to create Reaction with relations
prepareReactionWithRelations :: [M.Molecule]
                             -> [(C.Catalyst, Float, Float)]
                             -> R.Reaction
                             -> [(M.Molecule, Float)]
                             -> GraphPutRequest
prepareReactionWithRelations inM c reaction outM =
    flip execState emptyGraph $ do
        -- add reaction
        modify $ addNode "r" (CreateN . toNode $ reaction)
        -- add all input molecules
        traverse_ attachInMolecule inM
        -- add all input catalysts
        traverse_ attachCatalyst c
        -- add all output molecules
        traverse_ attachOutMolecule outM
    where
        attachInMolecule molecule =
            let n = pack $ "mIn" ++ show (M.id molecule)
            in  do
                modify $ addNode n (MergeN  . toNode $ molecule)
                modify $ addRelation n "r" (MergeR . toURelation $ REAGENT_IN)

        attachCatalyst (catalyst, temp, pressure) =
            let n = pack $  "c" ++ show (C.id catalyst)
            in  do
                modify $ addNode n (MergeN  . toNode $ catalyst)
                modify $ addRelation n "r" (MergeR . toURelation $ ACCELERATE temp pressure)

        attachOutMolecule (molecule, amount) =
            let n = pack $ "mOut" ++ show (M.id molecule)
            in  do
                modify $ addNode n (MergeN  . toNode $ molecule)
                modify $ addRelation "r" n (MergeR . toURelation $ PRODUCT_FROM amount)

putReactionWithRelations :: [M.Molecule]
                         -> [(C.Catalyst, Float, Float)]
                         -> R.Reaction
                         -> [(M.Molecule, Float)]
                         -> IO ()
putReactionWithRelations molecules catalysts reaction out = do
    request <- runQueryDB $ makeRequest @PutRequest []
                          $ prepareReactionWithRelations molecules catalysts reaction out
    print request


-- | Builds query to find Reaction
prepareReactionSearch :: Int -> GraphGetRequest
prepareReactionSearch id = flip execState emptyGraph $ do
        modify $ addNode "r" reaction
    where
        reaction = defaultNodeReturn & withLabelQ ''R.Reaction
                                     & withBoltId id
                                     & withReturn allProps

-- | Get Reaction from the database
findReaction :: Int -> IO ()
findReaction id = do
    request <- runQueryDB $ makeRequest @GetRequest []
                          $ prepareReactionSearch id
    let reaction :: [R.Reaction] = extractNode "r" <$> request
    print reaction


-- | Builds query to find shortest path between two Molecules
preparePathSearch :: Text -> Text -> BoltActionT IO [Path]
preparePathSearch m1 m2 = do
        records <- queryP q p
        forM records $ \record -> record `at` "path"
    where
        q =    "MATCH (m1:Molecule { iupacName: {props}.iupac1 }) "
            <> "MATCH (m2:Molecule { iupacName: {props}.iupac2 })"
            <> "MATCH path = shortestPath((m1)-[*]-(m2))"
            <> "RETURN path"
        p = props ["props" =: props ["iupac1" =: m1, "iupac2" =: m2]]

-- | Find shortest path between two Molecules
findPath :: M.Molecule -> M.Molecule -> IO ()
findPath m1 m2 = do
    path <- runQueryDB $
        preparePathSearch (pack $ M.iupacName m1) (pack $ M.iupacName m2)
    print path
