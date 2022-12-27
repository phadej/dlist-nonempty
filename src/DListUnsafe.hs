{-# LANGUAGE Trustworthy #-}
-- We export DList implementation from Trustworthy module
-- so the Internal module can stay Safe.
module DListUnsafe (DList (..)) where 
import Data.DList.Unsafe(DList (UnsafeDList, unsafeApplyDList))
