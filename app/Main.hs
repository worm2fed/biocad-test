module Main where

import           API                (putReactionWithRelations)
import           Type.Node.Catalyst (catalysts)
import           Type.Node.Molecule (molecules)
import           Type.Node.Reaction (reactions)


populate :: IO ()
populate = mapM_ (\(mIn, c, r, mOut) -> putReactionWithRelations mIn c r mOut)
                 $ helper reactions catalysts molecules
    where
        helper []     _      _             = []
        helper (r:rs) (c:cs) (m1:m2:m0:ms) =
            ( [m1, m2]
            , [(c, 36.6 * fromIntegral (length cs), 1.0 * fromIntegral (length cs))]
            , r
            , [(m0, 0.5 * fromIntegral (length rs))]
            ) : helper rs cs (m0:m1:ms)


main :: IO ()
main = populate
