{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Typed
  ( TypedValue,
    JSONType (..),
    type (.|.),
    ISO (..),

    -- ** Null
    null,
    getNull,
    Nullable,
    nullable,
    getNullable,

    -- ** Bool
    bool,
    getBool,

    -- ** String
    string,
    getString,

    -- ** Number
    number,
    getNumber,

    -- ** Array
    array,
    getArray,

    -- ** Object
    emptyObject,
    insert,
    getKey,

    -- ** Combinators
    left,
    getLeft,
    right,
    getRight,

    -- ** Deriving via helper
    UseAeson (..),
  )
where

import qualified Data.Aeson.Types as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude hiding (null)

-- | A 'TypedValue' is like an 'Aeson.Value' but tracks type information.
newtype TypedValue (t :: JSONType) = TypedValue {unTypedValue :: Aeson.Value}

-- | A restricted JSON type.
data JSONType
  = Object [(Symbol, JSONType)]
  | Array JSONType
  | String
  | Number
  | Bool
  | Null
  | --
    Or JSONType JSONType

-- | Isomorphism class.
class ISO (a :: Type) (t :: JSONType) | a -> t where
  toTypedValue :: a -> TypedValue t

  fromTypedValue :: TypedValue t -> a

null :: TypedValue 'Null
null = TypedValue Aeson.Null

getNull :: TypedValue 'Null -> ()
getNull = unsafeFromNullValue . unTypedValue

unsafeFromNullValue :: Aeson.Value -> ()
unsafeFromNullValue = \case
  Aeson.Null -> ()
  _ -> error "unsafeFromNullValue"

type Nullable t = t .|. 'Null

nullable :: Maybe (TypedValue t) -> TypedValue (Nullable t)
nullable = maybe (TypedValue Aeson.Null) coerceTypedValue

getNullable :: TypedValue (Nullable t) -> Maybe (TypedValue t)
getNullable = \case
  TypedValue Aeson.Null -> Nothing
  TypedValue t -> Just (TypedValue t)

bool :: Bool -> TypedValue 'Bool
bool = TypedValue . Aeson.Bool

getBool :: TypedValue 'Bool -> Bool
getBool = unsafeFromBoolValue . unTypedValue

unsafeFromBoolValue :: Aeson.Value -> Bool
unsafeFromBoolValue = \case
  Aeson.Bool b -> b
  _ -> error "unsafeFromBoolValue"

string :: Text -> TypedValue 'String
string = TypedValue . Aeson.String

getString :: TypedValue 'String -> Text
getString = unsafeFromStringValue . unTypedValue

unsafeFromStringValue :: Aeson.Value -> Text
unsafeFromStringValue = \case
  Aeson.String s -> s
  _ -> error "unsafeFromStringValue"

number :: Scientific -> TypedValue 'Number
number = TypedValue . Aeson.Number

getNumber :: TypedValue 'Number -> Scientific
getNumber = unsafeFromNumberValue . unTypedValue

unsafeFromNumberValue :: Aeson.Value -> Scientific
unsafeFromNumberValue = \case
  Aeson.Number n -> n
  _ -> error "unsafeFromNumberValue"

array :: Vector (TypedValue t) -> TypedValue ( 'Array t)
array = TypedValue . Aeson.Array . fmap unTypedValue

getArray :: TypedValue ( 'Array t) -> Vector (TypedValue t)
getArray = fmap TypedValue . unsafeFromArrayValue . unTypedValue

unsafeFromArrayValue :: Aeson.Value -> Vector Aeson.Value
unsafeFromArrayValue = \case
  Aeson.Array arr -> arr
  _ -> error "unsafeFromArrayValue"

emptyObject :: TypedValue ( 'Object '[])
emptyObject = TypedValue (Aeson.Object HashMap.empty)

insert ::
  forall s t fields.
  KnownSymbol s =>
  TypedValue t ->
  TypedValue ( 'Object fields) ->
  TypedValue ( 'Object ('(s, t) ': fields))
insert (TypedValue value) (TypedValue object) =
  TypedValue . Aeson.Object $
    HashMap.insert
      (symbolText (Proxy @s))
      value
      (unsafeFromObjectValue object)

getKey ::
  forall s fields.
  (KnownSymbol s) =>
  TypedValue ( 'Object fields) ->
  TypedValue (Lookup s fields)
getKey (TypedValue object) =
  TypedValue (unsafeFromObjectValue object HashMap.! symbolText (Proxy @s))

type family Lookup (x :: k) (l :: [(k, a)]) where
  Lookup k ('(k, a) ': _) = a
  Lookup k ('(_, _) ': ls) = Lookup k ls

unsafeFromObjectValue :: Aeson.Value -> HashMap Text Aeson.Value
unsafeFromObjectValue = \case
  Aeson.Object obj -> obj
  _ -> error "unsafeFromObjectValue"

type (.|.) = 'Or

left :: TypedValue t -> TypedValue (t .|. x)
left = coerceTypedValue

getLeft :: TypedValue (t .|. x) -> TypedValue t
getLeft = coerceTypedValue

right :: TypedValue t -> TypedValue (x .|. t)
right = coerceTypedValue

getRight :: TypedValue (x .|. t) -> TypedValue t
getRight = coerceTypedValue

coerceTypedValue :: TypedValue t -> TypedValue t'
coerceTypedValue = TypedValue . unTypedValue

symbolText :: KnownSymbol s => proxy s -> Text
symbolText = Text.pack . symbolVal

-- | Deriving via helper.
newtype UseAeson a (t :: JSONType) = UseAeson a

instance (ISO a t) => Aeson.ToJSON (UseAeson a t) where
  toJSON (UseAeson a) = unTypedValue (toTypedValue a)

instance (ISO a t, FromTypedValue t) => Aeson.FromJSON (UseAeson a t) where
  parseJSON raw = do
    json <- either fail pure $ parseTypedValue (Proxy @t) raw
    pure (UseAeson (fromTypedValue json))

class FromTypedValue (t :: JSONType) where
  parseTypedValue :: Proxy t -> Aeson.Value -> Either String (TypedValue t)

instance FromTypedValue 'Null where
  parseTypedValue _ = \case
    Aeson.Null -> pure null
    _ -> Left "not null"

instance FromTypedValue 'Bool where
  parseTypedValue _ = \case
    Aeson.Bool b -> pure (bool b)
    _ -> Left "not a bool"

instance FromTypedValue 'Number where
  parseTypedValue _ = \case
    Aeson.Number n -> pure (number n)
    _ -> Left "not a number"

instance FromTypedValue 'String where
  parseTypedValue _ = \case
    Aeson.String s -> pure (string s)
    _ -> Left "not a string"

instance FromTypedValue t => FromTypedValue ( 'Array t) where
  parseTypedValue _ = \case
    Aeson.Array arr -> array <$> traverse (parseTypedValue (Proxy @t)) arr
    _ -> Left "not an array"

instance FromTypedValueObject fields => FromTypedValue ( 'Object fields) where
  parseTypedValue _ = \case
    Aeson.Object obj ->
      TypedValue . Aeson.Object <$> parseTypedValueObject (Proxy @fields) obj HashMap.empty
    _ -> Left "not an object"

instance (FromTypedValue x, FromTypedValue y) => FromTypedValue ( 'Or x y) where
  parseTypedValue _ value =
    fmap coerceTypedValue (parseTypedValue (Proxy @x) value)
      <> fmap coerceTypedValue (parseTypedValue (Proxy @y) value)

class FromTypedValueObject (fields :: [(Symbol, JSONType)]) where
  parseTypedValueObject ::
    Proxy fields ->
    HashMap Text Aeson.Value ->
    HashMap Text Aeson.Value -> -- accum
    Either String (HashMap Text Aeson.Value)

instance FromTypedValueObject '[] where
  parseTypedValueObject _ _ accum = pure accum

instance (KnownSymbol k, FromTypedValue v, FromTypedValueObject rest) => FromTypedValueObject ('(k, v) : rest) where
  parseTypedValueObject _ obj accum = do
    let k = symbolText (Proxy @k)
    case HashMap.lookup k obj of
      Nothing -> Left ("key " <> show k <> " not present")
      Just v' ->
        case parseTypedValue (Proxy @v) v' of
          Left err -> Left ("key " <> show k <> " " <> err)
          Right v -> parseTypedValueObject (Proxy @rest) obj (HashMap.insert k (unTypedValue v) accum)
