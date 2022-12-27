{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,17,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
module GhcIsList (
  IsList (..),
) where

#if MIN_VERSION_base(4,17,0)
import GHC.IsList (IsList (..))
#else
import GHC.Exts (IsList (..))
#endif
