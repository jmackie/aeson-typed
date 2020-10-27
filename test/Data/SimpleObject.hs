{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.SimpleObject
  ( SimpleObject (..),
    Shape,
  )
where

import qualified Data.Aeson as Aeson
import Data.Aeson.Typed
import Data.Function ((&))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Prelude hiding (null)

data SimpleObject
  = SimpleObject
      { foo :: Bool,
        bar :: Vector Bool,
        baz :: Text,
        huh :: Maybe Scientific
      }
  deriving stock (Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (UseAeson SimpleObject Shape)

type Shape =
  'Object
    '[  '("foo", 'Bool),
        '("bar", 'Array 'Bool),
        '("baz", 'Object '[ '("why", 'String)]),
        '("huh", Nullable 'Number)
     ]

instance ISO SimpleObject Shape where

  toTypedValue SimpleObject {..} =
    emptyObject
      & insert @"foo" (bool foo)
      . insert @"bar" (array (fmap bool bar))
      . insert @"baz" (emptyObject & insert @"why" (string baz))
      . insert @"huh" (nullable (fmap number huh))

  fromTypedValue json = SimpleObject {..}
    where
      foo = getBool . getKey @"foo" $ json
      bar = fmap getBool . getArray . getKey @"bar" $ json
      baz = getString . getKey @"why" . getKey @"baz" $ json
      huh = fmap getNumber . getNullable . getKey @"huh" $ json
