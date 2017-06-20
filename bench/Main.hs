import Criterion.Main

import Prelude ()
import Prelude.Compat

import Data.List (foldl')
import Data.Foldable (toList)
import Data.Semigroup (Semigroup (..))
import Data.DList.Instances ()

import qualified Data.List.NonEmpty as NE
import qualified Data.DList as DList
import qualified Data.DList.NonEmpty as NEDList

sum' :: (Semigroup (f Int), Foldable f) => Int -> f Int -> Int
sum' m x = foldl' (+) 0 $ toList $ go x m
  where
    go y n | n <= 0    = y
           | otherwise = go (y <> x) (n - 1)

main :: IO ()
main = defaultMain
    [ bgroup "append 1000"
        [ bench "List"          $ whnf (sum' 1000) $ [1,2,3,4,5]
        , bench "NonEmpty"      $ whnf (sum' 1000) $ 1 NE.:| [2,3,4,5]
        , bench "DList"         $ whnf (sum' 1000) $ DList.fromList [1,2,3,4,5]
        , bench "NonEmptyDList" $ whnf (sum' 1000) $ NEDList.fromNonEmpty $ 1 NE.:| [2,3,4,5]
        ]
    ]
