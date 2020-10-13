{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800 && MIN_VERSION_dlist(1,0,0)
#define DNONEMPTY 1
#endif

module Main (main) where

import Criterion.Main

import Prelude ()
import Prelude.Compat

import Data.DList.Instances ()
import Data.Foldable        (toList)
import Data.List            (foldl')
import Data.Semigroup       (Semigroup (..))

import qualified Data.DList          as DList
import qualified Data.DList.NonEmpty as NEDList
import qualified Data.List.NonEmpty  as NE

#ifdef DNONEMPTY
import qualified Data.DList.DNonEmpty as DNonEmpty
#endif

sumRight :: (Semigroup (f Int), Foldable f) => Int -> f Int -> Int
sumRight m x = foldl' (+) 0 $ toList $ go x m
  where
    go y n | n <= 0    = y
           | otherwise = go (y <> x) (n - 1)

sumLeft :: (Semigroup (f Int), Foldable f) => Int -> f Int -> Int
sumLeft m x = foldl' (+) 0 $ toList $ go x m
  where
    go y n | n <= 0    = y
           | otherwise = go (x <> y) (n - 1)

sumTree :: (Semigroup (f Int), Foldable f) => Int -> f Int -> Int
sumTree m x = foldl' (+) 0 $ toList $ go m
  where
    go n | n <= 0    = x
         | otherwise = let y = go (n - 1) in y <> y

main :: IO ()
main = defaultMain
    -- left append is fast
    [ bgroup "left-append"
        [ bench "List"          $ whnf (sumLeft 1024) $ [1,2,3,4,5]
        , bench "NonEmpty"      $ whnf (sumLeft 1024) $ 1 NE.:| [2,3,4,5]
        , bench "DList"         $ whnf (sumLeft 1024) $ DList.fromList [1,2,3,4,5]
        , bench "NonEmptyDList" $ whnf (sumLeft 1024) $ NEDList.fromNonEmpty $ 1 NE.:| [2,3,4,5]
#ifdef DNONEMPTY
        , bench "DNonEmpty"     $ whnf (sumLeft 1024) $ DNonEmpty.fromNonEmpty $ 1 NE.:| [2,3,4,5]
#endif
        ]

    -- naive right append is quadratic for list
    , bgroup "right-append"
        [ bench "List"          $ whnf (sumRight 1024) $ [1,2,3,4,5]
        , bench "NonEmpty"      $ whnf (sumRight 1024) $ 1 NE.:| [2,3,4,5]
        , bench "DList"         $ whnf (sumRight 1024) $ DList.fromList [1,2,3,4,5]
        , bench "NonEmptyDList" $ whnf (sumRight 1024) $ NEDList.fromNonEmpty $ 1 NE.:| [2,3,4,5]
#ifdef DNONEMPTY
        , bench "DNonEmpty"     $ whnf (sumRight 1024) $ DNonEmpty.fromNonEmpty $ 1 NE.:| [2,3,4,5]
#endif
        ]

    -- not sure what to expect
    , bgroup "tree"
        [ bench "List"          $ whnf (sumTree 10) $ [1,2,3,4,5]
        , bench "NonEmpty"      $ whnf (sumTree 10) $ 1 NE.:| [2,3,4,5]
        , bench "DList"         $ whnf (sumTree 10) $ DList.fromList [1,2,3,4,5]
        , bench "NonEmptyDList" $ whnf (sumTree 10) $ NEDList.fromNonEmpty $ 1 NE.:| [2,3,4,5]
#ifdef DNONEMPTY
        , bench "DNonEmpty"     $ whnf (sumTree 10) $ DNonEmpty.fromNonEmpty $ 1 NE.:| [2,3,4,5]
#endif
        ]
    ]
