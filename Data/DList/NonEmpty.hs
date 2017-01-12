{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE TypeFamilies #-} -- For the IsList and IsString instances
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.NonEmptyDList.NonEmpty
-- Copyright   :  (c) 2006-2009 Don Stewart, 2013-2016 Sean Leather, 2017 Oleg Grenrus
-- License     :  See LICENSE file
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  stable
-- Portability :  portable
--
-- Non-empty difference lists: a data structure for /O(1)/ append on lists.
--
-----------------------------------------------------------------------------

module Data.DList.NonEmpty
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
  ( NonEmptyDList(Cons)
#else
  ( NonEmptyDList
#endif

  -- * Construction
  , fromNonEmpty
  , toNonEmpty
  , toList
  , toDList
  , apply

  -- * Basic functions
  , singleton
  , cons
  , snoc
  , append
  , concat1
  , replicate
  , head
  , tail
  , unfoldr
  , map

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
  -- * Pattern Synonyms
  , pattern Cons
#endif

  ) where

import Prelude ()
import Prelude.Compat hiding (concat, foldr, map, head, tail, replicate)

import Control.DeepSeq (NFData (..))
import Data.Function (on)
import Data.String (IsString(..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup(..))
import Control.Monad

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as F
import qualified Data.DList as DList

-- semigroupoids
import Data.Functor.Apply (Apply (..))
import Data.Functor.Bind (Bind (..))
import Data.Functor.Alt (Alt (..))
import Data.Semigroup.Foldable (Foldable1 (..))
import Data.Semigroup.Traversable (Traversable1 (..))

#ifdef __GLASGOW_HASKELL__

import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec,
                  readListPrecDefault)

#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (IsList)
-- This is for the IsList methods, which conflict with fromList, toList:
import qualified GHC.Exts
#endif

#endif

-- | A difference list is a function that, given a list, returns the original
-- contents of the difference list prepended to the given list.
newtype NonEmptyDList a = DL { unDL :: [a] -> NonEmpty a }

-- | Convert a list to a dlist
fromNonEmpty :: NonEmpty a -> NonEmptyDList a
fromNonEmpty (x :| xs) = DL $ (x :|) . (xs ++)
{-# INLINE fromNonEmpty #-}

-- | Convert a dlist to a non-empty list
toNonEmpty :: NonEmptyDList a -> NonEmpty a
toNonEmpty = ($[]) . unDL
{-# INLINE toNonEmpty #-}

-- | Convert a dlist to a list
toList :: NonEmptyDList a -> [a]
toList = NE.toList . toNonEmpty
{-# INLINE toList #-}

-- | @dlist@ doesn't expose internals, so this have to go through list.
toDList :: NonEmptyDList a -> DList.DList a
toDList = DList.fromList . toList
{-# INLINE toDList #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
-- | A unidirectional pattern synonym using 'toList' in a view pattern and
-- matching on @x:xs@ such that you have the pattern @Cons x xs@
#if __GLASGOW_HASKELL__ >= 710
pattern Cons :: a -> [a] -> NonEmptyDList a
#endif
pattern Cons x xs <- (toNonEmpty -> x :| xs)
#endif

-- | Apply a dlist to a list to get the underlying non-empty list with an extension
apply :: NonEmptyDList a -> [a] -> NonEmpty a
apply = unDL

-- | Create dlist with a single element
singleton :: a -> NonEmptyDList a
singleton = DL . (:|)
{-# INLINE singleton #-}

-- | /O(1)/. Prepend a single element to a dlist
infixr `cons`
cons :: a -> NonEmptyDList a -> NonEmptyDList a
cons x xs = DL (NE.cons x . unDL xs)
{-# INLINE cons #-}

-- | /O(1)/. Append a single element to a dlist
infixl `snoc`
snoc :: NonEmptyDList a -> a -> NonEmptyDList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- | /O(1)/. Append dlists
append :: NonEmptyDList a -> NonEmptyDList a -> NonEmptyDList a
append xs ys = DL (unDL xs . NE.toList . unDL ys)
{-# INLINE append #-}

-- | /O(spine)/. Concatenate dlists
concat1 :: NonEmpty (NonEmptyDList a) -> NonEmptyDList a
concat1 = sconcat
{-# INLINE concat1 #-}

-- | /O(n)/. Create a dlist of the given number of elements.
--
-- Always create list with at least one element.
replicate :: Int -> a -> NonEmptyDList a
replicate n x = DL $ \xs -> let go m | m <= 1 = x :| xs
                                     | otherwise = NE.cons x $ go (m-1)
                            in go n
{-# INLINE replicate #-}

-- | /O(n)/. Return the head of the dlist
head :: NonEmptyDList a -> a
head = NE.head . toNonEmpty

-- | /O(n)/. Return the tail of the dlist
tail :: NonEmptyDList a -> [a]
tail = NE.tail . toNonEmpty

-- | /O(n)/. Unfoldr for dlists
unfoldr :: (b -> (a, Maybe b)) -> b -> NonEmptyDList a
unfoldr pf b = case pf b of
    (a, Nothing) -> singleton a
    (a, Just b')  -> cons a (unfoldr pf b')

-- | /O(n)/. Map over difference lists.
map :: (a -> b) -> NonEmptyDList a -> NonEmptyDList b
map f = fromNonEmpty . fmap f . toNonEmpty
{-# INLINE map #-}

instance Eq a => Eq (NonEmptyDList a) where
  (==) = (==) `on` toList

instance Ord a => Ord (NonEmptyDList a) where
  compare = compare `on` toList

-- The Read and Show instances were adapted from Data.Sequence.

instance Read a => Read (NonEmptyDList a) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromNonEmpty" <- lexP
    dl <- readPrec
    return (fromNonEmpty dl)
  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromNonEmpty", s) <- lex r
    (dl, t) <- readsPrec 11 s
    return (fromNonEmpty dl, t)
#endif

instance Show a => Show (NonEmptyDList a) where
  showsPrec p dl = showParen (p > 10) $
    showString "fromNonEmpty " . showsPrec 11 (toNonEmpty dl)

instance Functor NonEmptyDList where
  fmap = map
  {-# INLINE fmap #-}

instance Applicative NonEmptyDList where
  pure = singleton
  {-# INLINE pure #-}
  (<*>) = ap

instance Monad NonEmptyDList where
  m >>= k
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = List.foldr append empty . List.map k . toList $ m
    -- = List.foldr (append . k) empty . toList $ m
   = concat1 . fmap k . toNonEmpty $ m
  {-# INLINE (>>=) #-}

  return = pure
  {-# INLINE return #-}

instance Foldable NonEmptyDList where
  fold = mconcat . toList
  {-# INLINE fold #-}

  foldMap f = F.foldMap f . toList
  {-# INLINE foldMap #-}

  foldr f x = List.foldr f x . toList
  {-# INLINE foldr #-}

  foldl f x = List.foldl f x . toList
  {-# INLINE foldl #-}

  foldr1 f = List.foldr1 f . toList
  {-# INLINE foldr1 #-}

  foldl1 f = List.foldl1 f . toList
  {-# INLINE foldl1 #-}

-- CPP: foldl', foldr' added to Foldable in 7.6.1
-- http://www.haskell.org/ghc/docs/7.6.1/html/users_guide/release-7-6-1.html
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
  foldl' f x = List.foldl' f x . toList
  {-# INLINE foldl' #-}

  foldr' f x = F.foldr' f x . toList
  {-# INLINE foldr' #-}
#endif

instance Traversable NonEmptyDList where
  traverse f = fmap fromNonEmpty . traverse f . toNonEmpty
  sequenceA  = fmap fromNonEmpty . sequenceA . toNonEmpty

instance NFData a => NFData (NonEmptyDList a) where
  rnf = rnf . toNonEmpty
  {-# INLINE rnf #-}

-- This is partial instance. Will fail on empty string.
instance a ~ Char => IsString (NonEmptyDList a) where
  fromString = fromNonEmpty . NE.fromList
  {-# INLINE fromString #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
instance IsList (NonEmptyDList a) where
  type Item (NonEmptyDList a) = a
  fromList = fromNonEmpty . NE.fromList
  {-# INLINE fromList #-}
  toList = toList
  {-# INLINE toList #-}
#endif

instance Semigroup (NonEmptyDList a) where
  (<>) = append
  {-# INLINE (<>) #-}

-------------------------------------------------------------------------------
-- semigroupoids
-------------------------------------------------------------------------------

instance Apply NonEmptyDList where (<.>) = (<*>)
instance Bind NonEmptyDList where (>>-) = (>>=)

instance Foldable1 NonEmptyDList where
  foldMap1 f = foldMap1 f . toNonEmpty

instance Traversable1 NonEmptyDList where
  traverse1 f = fmap fromNonEmpty . traverse1 f . toNonEmpty
  sequence1   = fmap fromNonEmpty . sequence1 . toNonEmpty

instance Alt NonEmptyDList where
  (<!>) = append
