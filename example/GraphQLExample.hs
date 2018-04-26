{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module GraphQLExample where

import Protolude

import Data.Row.Poly
       (type (.!), type (.==), type (≈), KnownSymbol,
        Label(..))
import qualified Data.Row.Variants as Variant
import GraphQL.Resolver
import GraphQL.Type
import qualified GraphQL.Type.QuasiQuoter as GraphQL
import qualified GraphQL.Type.Scalars as Scalars
import qualified Unsafe.Coerce as UNSAFE (unsafeCoerce)

dummyText :: Text
dummyText = "hello world"

scalarSchema :: SchemaResolver IO Scalars.Definitions
scalarSchema = fix (SchemaResolver . Scalars.resolver)

data SchemaPlaceholder

data TypePlaceholder

data FieldPlaceholder

data InputValuePlaceholder

data EnumValuePlaceholder

data DirectivePlaceholder

type IntrospectionDefinitions
   = [GraphQL.schema|

"""
A GraphQL schema defines the capabilities of a GraphQL server. It
exposes all avilable types and directives on the server, as well as
the entry points for query, mutation, and subscription operations.
"""
type __Schema @haskellType(name: "SchemaPlaceholder") {
  """
  A list of all types supported by this server.
  """
  types: [__Type!]!

  """
  The type that query operations will be rooted at.
  """
  queryType: __Type!

  """
  If this server supports mutation, the type that
  mutation operations will be rooted at.
  """
  mutationType: __Type

  """
  If this server supports subscription, the type that
  mutation operations will be rooted at.
  """
  subscriptionType: __Type

  """
  A list of all directives supported by this server.
  """
  directives: [__Directive!]!
}

"""
The fundamental unit of any GraphQL Schema is the type. There are
many kinds of types in GraphQL as represented by the `__TypeKind` enum.

Depending on the kind of a type, certain fields describe
information about that type. Scalar types provide no information
beyond a name and description, while Enum types provide their values.
Object and Interface types provide the fields they describe. Abstract
types, Union and Interface, provide the Object types possible
at runtime. List and NonNull types compose other types.
"""

type __Type @haskellType(name: "TypePlaceholder") {
  kind: __TypeKind!
  name: String
  description: String

  "OBJECT and INTERFACE only"
  fields(includeDeprecated: Boolean = false): [__Field!]

  "OBJECT only"
  interfaces: [__Type!]

  "INTERFACE and UNION only"
  possibleTypes: [__Type!]

  "ENUM only"
  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]

  "INPUT_OBJECT only"
  inputFields: [__InputValue!]

  "NON_NULL and LIST only"
  ofType: __Type
}

"""
Object and Interface types are described by a list of Fields, each of
which has a name, potentially a list of arguments, and a return type.
"""
type __Field @haskellType(name: "FieldPlaceholder") {
  name: String!
  description: String
  args: [__InputValue!]!
  type: __Type!
  isDeprecated: Boolean!
  deprecationReason: String
}

"""
Arguments provided to Fields or dierctives and the input fields of an
InputObject are represented as Input Values which describe their type
and optionally a default value.
"""
type __InputValue @haskellType(name: "InputValuePlaceholder") {
  name: String!
  description: String
  type: __Type!

  """
  A GraphQL-formatted string representing the default value for
  this input value.
  """
  defaultValue: String
}

"""
One possible value for a given Enum. Enum values are unique values, not
a placeholder for a string or numeric value. However an Enum value is
returned in a JSON response as a string.
"""
type __EnumValue @haskellType(name: "EnumValuePlaceholder") {
  name: String!
  description: String
  isDeprecated: Boolean!
  deprecationReason: String
}


"""
An enum describing what kind of type a given `__Type` is.
"""
enum __TypeKind {
  """
  Indicates this type is a scalar.
  """
  SCALAR

  """
  Indicates this type is an object.
  `fields` and `interfaces` are valid fields.
  """
  OBJECT

  """
  Indicates this type is an interface.
  `fields` and `possibleTypes` are valid fields.
  """
  INTERFACE

  """
  Indicates this type is a union.
  `possibleTypes` is a valid field.
  """
  UNION

  """
  Indicates this type is an enum.
  `enumValues` is a valid field.
  """
  ENUM

  """
  Indicates this type is an input object.
  `inputFields` is a valid field.
  """
  INPUT_OBJECT

  """
  Indicates this type is a list.
  `ofType` is a valid field.
  """
  LIST

  """
  Indicates this type is a non-null.
  `ofType` is a valid field.
  """
  NON_NULL
}


"""
A Directive provides a way to describe alternate runtime execution and
type validation behavior in a GraphQL document.

In some cases, you need to provide options to alter GraphQL's
execution behavior in ways field arguments will not suffice, such as
conditionally including or skipping a field. Directives provide this by
describing additional information to the executor.
"""
type __Directive @haskellType(name: "DirectivePlaceholder") {
  name: String!
  description: String
  locations: [__DirectiveLocation!]!
  args: [__InputValue!]!
  onOperation: Boolean! @deprecated(reason: "Use `locations`")
  onFragment: Boolean! @deprecated(reason: "Use `locations`")
  onField: Boolean! @deprecated(reason: "Use `locations`")
}


"""
A Directive can be adjacent to many parts of the GraphQL language, a
__DirectiveLocation describes one such possible adjacencies.
"""
enum __DirectiveLocation {
  "Location adjacent to a query operation."
  QUERY

  "Location adjacent to a mutation operation."
  MUTATION

  "Location adjacent to a subscription operation."
  SUBSCRIPTION

  "Location adjacent to a field."
  FIELD

  "Location adjacent to a fragment definition."
  FRAGMENT_DEFINITION

  "Location adjacent to a fragment spread."
  FRAGMENT_SPREAD

  "Location adjacent to an inline fragment."
  INLINE_FRAGMENT

  "Location adjacent to a schema definition."
  SCHEMA

  "Location adjacent to a scalar definition."
  SCALAR

  "Location adjacent to an object type definition."
  OBJECT

  "Location adjacent to a field definition."
  FIELD_DEFINITION

  "Location adjacent to an argument definition."
  ARGUMENT_DEFINITION

  "Location adjacent to an interface definition."
  INTERFACE

  "Location adjacent to a union definition."
  UNION

  "Location adjacent to an enum definition."
  ENUM

  "Location adjacent to an enum value definition."
  ENUM_VALUE

  "Location adjacent to an input object type definition."
  INPUT_OBJECT

  "Location adjacent to an input object field definition."
  INPUT_FIELD_DEFINITION
}

|]

type DummyType = ()

type Schema = [GraphQL.schema|
  type Unit @haskellType(name: "DummyType") {
    unit: Unit!
  }

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

type DummyInterfaceT = GetType Schema ('NON_NULL "DummyInterface1")

unionValue ::
     forall label value row. (KnownSymbol label, row .! label ≈ value)
  => value
  -> Variant.Var row
unionValue = diversify . Variant.singleton Label
  where
    diversify :: Variant.Var (label .== value) -> Variant.Var row
    diversify = UNSAFE.unsafeCoerce

value :: Applicative m => m (Maybe [Maybe DummyInterfaceT])
value = pure . pure . pure . pure $ unionValue @"DummyType1" ()
