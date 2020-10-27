{-# LANGUAGE BlockArguments #-}

module Main
  ( main,
  )
where

import qualified SerializeSpec
import Test.Hspec
import Prelude

main :: IO ()
main = hspec do
  describe "Serialize" SerializeSpec.spec
