{-# LANGUAGE OverloadedStrings #-}

module OverloadedStrings (testOverloadedStrings) where

import Data.DList.NonEmpty

testOverloadedStrings :: IO ()
testOverloadedStrings = print $ "OverloadedStrings:" `append` " success"
