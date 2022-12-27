-- | This module exports the internal representation of 'NonEmptyDList'.
--
-- Use with care. It's very easy to break the safe interface:
--
-- >>> let nedl = NEDL ((1 :|) . map (+1))
-- >>> nedl <> nedl
-- fromNonEmpty (1 :| [2])
--
{-# LANGUAGE Unsafe #-}
module Data.DList.NonEmpty.Unsafe (NonEmptyDList (..)) where

import Data.DList.NonEmpty.Internal
