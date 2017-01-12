{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE OverloadedLists #-} -- For the IsList test
#if __GLASGOW_HASKELL__ == 708
{-# LANGUAGE PatternSynonyms #-} -- For pattern synonym use only in GHC 7.8
#endif
#endif


--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Prelude hiding (concat, foldr, head, map, replicate, tail)
import qualified Data.List as List
import Test.QuickCheck
import Test.QuickCheck.Function

import Data.DList.NonEmpty

import OverloadedStrings (testOverloadedStrings)

import Data.Semigroup (Semigroup(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------

eqWith :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
eqWith f g x = f x == g x

eqOn :: Eq b => (a -> Bool) -> (a -> b) -> (a -> b) -> a -> Property
eqOn c f g x = c x ==> f x == g x

--------------------------------------------------------------------------------

prop_model :: NonEmpty Int -> Bool
prop_model = eqWith id (toNonEmpty . fromNonEmpty)

prop_singleton :: Int -> Bool
prop_singleton = eqWith (:|[]) (toNonEmpty . singleton)

prop_cons :: Int -> NonEmpty Int -> Bool
prop_cons c = eqWith (NE.cons c) (toNonEmpty . cons c . fromNonEmpty)

prop_snoc :: NonEmpty Int -> Int -> Bool
prop_snoc xs c = xs <> [c] == toNonEmpty (snoc (fromNonEmpty xs) c)

prop_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_append xs ys = xs <> ys == toNonEmpty (fromNonEmpty xs `append` fromNonEmpty ys)

prop_concat1 :: NonEmpty (NonEmpty Int) -> Bool
prop_concat1 = eqWith sconcat (toNonEmpty . concat1 . fmap fromNonEmpty)

-- The condition reduces the size of replications and thus the eval time.
prop_replicate :: Int -> Int -> Property
prop_replicate n =
  eqOn (const (n < 100)) (rep n) (toNonEmpty . replicate n)
  where
    rep m x = x :| List.replicate (m - 1) x

prop_head :: NonEmpty Int -> Property
prop_head = eqOn (not . null) NE.head (head . fromNonEmpty)

prop_tail :: NonEmpty Int -> Property
prop_tail = eqOn (not . null) NE.tail (tail . fromNonEmpty)

prop_unfoldr :: Fun Int (Int, Maybe Int) -> Int -> Int -> Property
prop_unfoldr (Fun _ f) n =
  eqOn (const (n >= 0)) (NE.take n . NE.unfoldr f) (NE.take n . toNonEmpty . unfoldr f)

prop_map :: Fun Int Int -> NonEmpty Int -> Bool
prop_map (Fun _ f) = eqWith (fmap f) (toNonEmpty . map f . fromNonEmpty)

prop_map_fusion :: (Int -> Int) -> (a -> Int) -> NonEmpty a -> Bool
prop_map_fusion f g =
  eqWith (fmap f . fmap g) (toNonEmpty . map f . map g . fromNonEmpty)

prop_show_read :: [Int] -> Bool
prop_show_read = eqWith id (read . show)

prop_read_show :: NonEmpty Int -> Bool
prop_read_show x = eqWith id (show . f . read) $ "fromNonEmpty (" <> show x <> ")"
  where
    f :: NonEmptyDList Int -> NonEmptyDList Int
    f = id

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
-- | Test that the IsList instance methods compile and work with simple lists
prop_IsList :: Bool
prop_IsList = test_fromNonEmpty [1,2,3] && test_toNonEmpty (fromNonEmpty [1,2,3])
  where
    test_fromNonEmpty, test_toNonEmpty :: NonEmptyDList Int -> Bool
    test_fromNonEmpty x = x == fromNonEmpty [1,2,3]
    test_toNonEmpty [1,2,3] = True
    test_toNonEmpty _       = False

prop_patterns :: NonEmpty Int -> Bool
prop_patterns xs = case fromNonEmpty xs of
  Cons y ys -> xs == (y:|ys)
  _         -> False
#endif

prop_Semigroup_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_Semigroup_append xs ys = xs <> ys == toNonEmpty (fromNonEmpty xs <> fromNonEmpty ys)

prop_Semigroup_sconcat :: NonEmpty (NonEmpty Int) -> Bool
prop_Semigroup_sconcat xs = sconcat xs == toNonEmpty (sconcat (fmap fromNonEmpty xs))

prop_Semigroup_stimes :: Int -> NonEmpty Int -> Bool
prop_Semigroup_stimes n xs = n < 1 ||
    stimes n xs == toNonEmpty (stimes n (fromNonEmpty xs))

--------------------------------------------------------------------------------

props :: [(String, Property)]
props =
  [ ("model",             property prop_model)
  , ("singleton",         property prop_singleton)
  , ("cons",              property prop_cons)
  , ("snoc",              property prop_snoc)
  , ("append",            property prop_append)
  , ("concat1",           property prop_concat1)
  , ("replicate",         property prop_replicate)
  , ("head",              property prop_head)
  , ("tail",              property prop_tail)
  , ("unfoldr",           property prop_unfoldr)
  , ("map",               property prop_map)
  , ("map fusion",        property (prop_map_fusion (+1) (+1)))
  , ("read . show",       property prop_show_read)
  , ("show . read",       property prop_read_show)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
  , ("IsList",            property prop_IsList)
  , ("patterns",          property prop_patterns)
#endif
#if MIN_VERSION_base(4,9,0)
  , ("Semigroup <>",      property prop_Semigroup_append)
  , ("Semigroup sconcat", property prop_Semigroup_sconcat)
  , ("Semigroup stimes",  property prop_Semigroup_stimes)
#endif
  ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  testOverloadedStrings
  quickCheck $ conjoin $ fmap (uncurry label) props
