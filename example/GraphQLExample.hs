{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module GraphQLExample where

import Protolude

import qualified Data.Row.Variants as V
import Data.Row.Poly
       (type (.!), type (.==), type (.+), type (≈), KnownSymbol,
        Label(..))
import qualified Data.Row.Variants as Variant
import GraphQL.Resolver
import GraphQL.Type
import qualified GraphQL.Type.QuasiQuoter as GraphQL
import qualified GraphQL.Type.Scalars as Scalars
import qualified GraphQL.Type.Introspection as Introspection
import qualified Unsafe.Coerce as UNSAFE (unsafeCoerce)

dummyText :: Text
dummyText = "hello world"

scalarSchema :: SchemaResolver IO Scalars.Definitions
scalarSchema = fix (SchemaResolver . Scalars.resolver)


type DummyType = ()

type Schema = Scalars.Definitions .+ Introspection.Definitions .+ [GraphQL.schema|
  type Unit @haskellType(name: "DummyType") {
    unit: Unit!
  }

  union DummyUnion =
    | __Type
    | __Schema
    | __Field

  interface DummyInterface1 {
    foo: Unit
  }

  interface DummyInterface2 {
    bar: Unit
  }

  type DummyType1 implements
    & DummyInterface1
    @haskellType(name: "DummyType") {
    foo: Unit!
  }

  type DummyType2 implements
    & DummyInterface2
    @haskellType(name: "DummyType") {
    bar: Unit
  }

  type DummyType3 implements
    & DummyInterface1
    & DummyInterface2
    @haskellType(name: "DummyType") {
    bar: Unit!
  }
|]

type DummyT = GetType Schema ('NULLABLE_LIST_OF ('NULLABLE "Unit"))

unionValue ::
     forall label value row. (KnownSymbol label, row .! label ≈ value)
  => value
  -> Variant.Var row
unionValue = diversify . Variant.singleton Label
  where
    diversify :: Variant.Var (label .== value) -> Variant.Var row
    diversify = UNSAFE.unsafeCoerce

type GoalT = V.Var ("DummyType1" .== () .+ "DummyType3" .== ())

constrained :: GetNamedType schema name ~ GoalT => GetType schema ('NULLABLE_LIST_OF ('NULLABLE name))
constrained = pure . pure . pure $ unionValue @"DummyType1" ()

-- value :: Applicative m => m DummyT
-- value = pure . pure . pure . pure $ ()
