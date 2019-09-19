{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif

-- | Non-empty difference lists: a data structure for /O(1)/ append on non-empty lists.
module Data.DList.NonEmpty
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
  ( NonEmptyDList(Cons)
#else
  ( NonEmptyDList
#endif

  -- * Conversion
  , toNonEmpty
  , apply
  , toList
  , toDList
  , toEndo
  , toEndo'

  -- * Construction
  --
  -- | The O(1) functions.
  , fromNonEmpty
  , singleton
  , cons
  , snoc
  , append

  -- * Other functions
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
import Data.DList.NonEmpty.Internal
