{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module SerializeSpec
  ( spec,
  )
where

import qualified Data.Aeson as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.Aeson.Types as Aeson
import Data.SimpleObject
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "SimpleObject" do
    it "encodes as expected" do
      Aeson.toJSON SimpleObject {foo = True, bar = [False, False], baz = "noice", huh = Nothing}
        `shouldBe` [aesonQQ| { "foo": true, "bar": [false, false], "baz": { "why": "noice" }, "huh": null } |]
    it "roundtrips" do
      let so = SimpleObject {foo = True, bar = [False, False], baz = "noice", huh = Nothing}
      Aeson.parseEither Aeson.parseJSON (Aeson.toJSON so) `shouldBe` Right so
    it "fails to deserialize as expected" do
      Aeson.parseEither @Aeson.Value @SimpleObject
        Aeson.parseJSON
        [aesonQQ| {} |]
        `shouldBe` Left "Error in $: key \"foo\" not present"
      Aeson.parseEither @Aeson.Value @SimpleObject
        Aeson.parseJSON
        [aesonQQ| { "foo": 2 } |]
        `shouldBe` Left "Error in $: key \"foo\" not a bool"
      Aeson.parseEither @Aeson.Value @SimpleObject
        Aeson.parseJSON
        [aesonQQ| { "foo": true, "bar": [], "baz": null } |]
        `shouldBe` Left "Error in $: key \"baz\" not an object"
      Aeson.parseEither @Aeson.Value @SimpleObject
        Aeson.parseJSON
        [aesonQQ| { "foo": true, "bar": [], "baz": { "why": null } } |]
        `shouldBe` Left "Error in $: key \"baz\" key \"why\" not a string"
