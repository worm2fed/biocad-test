{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module API (runQueryDB, putCatalyst, putMolecule, putReaction, putReactionWithRelations, findReaction, findPath) where

import           Control.Exception          (bracket)
import           Control.Monad.State        (execState, modify)
import           Data.Default               (Default (def))
import           Data.Function              ((&))

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

import           Control.Monad              (forM)
import           Data.Text                  (Text, pack)
import           Type.Node.Catalyst         (Catalyst)
import           Type.Node.Molecule         (Molecule, iupacName)
import           Type.Node.Reaction         (Reaction)
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
prepareCatalyst :: Catalyst -> GraphPutRequest
prepareCatalyst catalyst = flip execState emptyGraph $ do
        modify $ addNode "c" (CreateN . toNode $ catalyst)

-- | Put Catalyst to the database
putCatalyst :: Catalyst -> IO ()
putCatalyst catalyst = do
    request <- runQueryDB $ makeRequest @PutRequest []
                          $ prepareCatalyst catalyst
    print request


-- | Builds query to create Molecule
prepareMolecule :: Molecule -> GraphPutRequest
prepareMolecule molecule = flip execState emptyGraph $ do
        modify $ addNode "m" (CreateN . toNode $ molecule)

-- | Put Molecule to the database
putMolecule :: Molecule -> IO ()
putMolecule molecule = do
    request <- runQueryDB $ makeRequest @PutRequest []
                          $ prepareMolecule molecule
    print request


-- | Builds query to create Reaction
prepareReaction :: Reaction -> GraphPutRequest
prepareReaction reaction = flip execState emptyGraph $ do
        modify $ addNode "r" (CreateN . toNode $ reaction)

-- | Put Reaction to the database
putReaction :: Reaction -> IO ()
putReaction reaction = do
    request <- runQueryDB $ makeRequest @PutRequest []
                          $ prepareReaction reaction
    print request


-- | Builds query to create Reaction with relations
prepareReactionWithRelations :: (Molecule, Molecule)
                             -> (Catalyst, Float, Float)
                             -> (Reaction, Float)
                             -> Molecule
                             -> GraphPutRequest
prepareReactionWithRelations (m1, m2) (catalyst, temp, pressure) (reaction, amount) out =
    flip execState emptyGraph $ do
        modify $ addNode "r" (CreateN . toNode $ reaction)

        modify $ addNode "m1" (MergeN  . toNode $ m1)
        modify $ addNode "m2" (MergeN  . toNode $ m2)

        modify $ addNode "c" (MergeN  . toNode $ catalyst)

        modify $ addNode "m" (MergeN  . toNode $ out)

        modify $ addRelation "m1" "r" (MergeR . toURelation $ REAGENT_IN)
        modify $ addRelation "m2" "r" (MergeR . toURelation $ REAGENT_IN)

        modify $ addRelation "c" "r" (MergeR . toURelation $ ACCELERATE temp pressure)

        modify $ addRelation "r" "m" (MergeR . toURelation $ PRODUCT_FROM amount)

putReactionWithRelations:: (Molecule, Molecule)
               -> (Catalyst, Float, Float)
               -> (Reaction, Float)
               -> Molecule
               -> IO ()
putReactionWithRelations molecules catalyst reaction out = do
    request <- runQueryDB $ makeRequest @PutRequest []
                          $ prepareReactionWithRelations molecules catalyst reaction out
    print request


-- | Builds query to find Reaction
prepareReactionSearch :: Int -> GraphGetRequest
prepareReactionSearch id = flip execState emptyGraph $ do
        modify $ addNode "r" reaction
    where
        reaction = defaultNodeReturn & withLabelQ ''Reaction
                                     & withBoltId id
                                     & withReturn allProps

-- | Get Reaction from the database
findReaction :: Int -> IO ()
findReaction id = do
    request <- runQueryDB $ makeRequest @GetRequest []
                          $ prepareReactionSearch id
    let reaction :: [Reaction] = extractNode "r" <$> request
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
findPath :: Molecule -> Molecule -> IO ()
findPath m1 m2 = do
    path <- runQueryDB $
        preparePathSearch (pack $ iupacName m1) (pack $ iupacName m2)
    print path
