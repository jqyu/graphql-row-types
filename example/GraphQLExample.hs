{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}


module GraphQLExample where

import Protolude

import Data.Row.Poly (Empty, Label(..), KnownSymbol, type (.==), type (.+), type (.!), type (≈))
import qualified Data.Row.Variants as Variant
import GraphQL.Type
import qualified GraphQL.Type.Scalars as Scalars
import GraphQL.Resolver
import qualified Unsafe.Coerce as UNSAFE (unsafeCoerce)


newtype Id = Id { unId :: Text } deriving (Eq, Ord, Show)

scalarSchema :: SchemaResolver IO Scalars.Definitions
scalarSchema = fix (SchemaResolver . Scalars.resolver)


data SchemaPlaceholder
data TypePlaceholder
data FieldPlaceholder
data InputValuePlaceholder
data EnumValuePlaceholder
data DirectivePlaceholder

type SchemaDefinition =
  "__Schema" .==
    'OBJECT SchemaPlaceholder
      "A GraphQL Schema defines the capabilities of a GraphQL server. It \
      \exposes all available types and directives on the server, as well as \
      \the entry points for query, mutation, and subscription operations."
      '[]
      ( "types" .==
          'FIELD Empty ('NON_NULL_LIST_OF ('NON_NULL "__Type"))
            "A list of all types supported by this server."
      .+ "queryType" .==
          'FIELD Empty ('NON_NULL "__Type")
            "The type that query operations will be rooted at."
      .+ "mutationType" .==
          'FIELD Empty ('NULLABLE "__Type")
            "If this server supports mutation, the type that \
            \mutation operations will be rooted at."
      .+ "subscriptionType" .==
          'FIELD Empty ('NULLABLE "__Type")
            "If this server supports subscription, the type that \
            \mutation operations will be rooted at."
      .+ "directives" .==
          'FIELD Empty ('NON_NULL_LIST_OF ('NON_NULL "__Type"))
            "A list of all directives supported by this server."
      )

type DirectiveDefinition =
  "__Directive" .==
    'OBJECT DirectivePlaceholder
      "A Directive provides a way to describe alternate runtime execution and \
      \type validation behavior in a GraphQL document.\
      \\n\nIn some cases, you need to provide options to alter GraphQL's \
      \execution behavior in ways field arguments will not suffice, such as \
      \conditionally including or skipping a field. Directives provide this by \
      \describing additional information to the executor."
      '[]
      ( "name" .==
          'FIELD Empty ('NON_NULL "String") ""
      .+ "description" .==
          'FIELD Empty ('NULLABLE "String") ""
      .+ "locations" .==
          'FIELD Empty ('NON_NULL_LIST_OF ('NON_NULL "__DirectiveLocation")) ""
      .+ "args" .==
          'FIELD Empty ('NON_NULL_LIST_OF ('NON_NULL "__InputValue")) ""
      -- NOTE: the following three fields are deprecated and are no longer part
      -- of the GraphQL specification.
      .+ "onOperation" .==
          'DEPRECATED_FIELD Empty ('NON_NULL "Boolean") ""
            "Use `locations`."
      .+ "onFragment" .==
          'DEPRECATED_FIELD Empty ('NON_NULL "Boolean") ""
            "Use `locations`."
      .+ "onField" .==
          'DEPRECATED_FIELD Empty ('NON_NULL "Boolean") ""
            "Use `locations`."
      )

type DirectiveLocationDefinition =
  "__DirectiveLocation" .==
    'ENUM
      "A Directive can be adjacent to many parts of the GraphQL language, a \
      \__DirectiveLocation describes one such possible adjacencies."
      ( "QUERY" .==
          'ENUM_VALUE "Location adjacent to a query operation."
      .+ "MUTATION" .==
          'ENUM_VALUE "Location adjacent to a mutation operation."
      .+ "SUBSCRIPTION" .==
          'ENUM_VALUE "Location adjacent to a subscription operation."
      .+ "FIELD" .==
          'ENUM_VALUE "Location adjacent to a field."
      .+ "FRAGMENT_DEFINITION" .==
          'ENUM_VALUE "Location adjacent to a fragment definition."
      .+ "FRAGMENT_SPREAD" .==
          'ENUM_VALUE "Location adjacent to a fragment spread."
      .+ "INLINE_FRAGMENT" .==
          'ENUM_VALUE "Location adjacent to an inline fragment."
      .+ "SCHEMA" .==
          'ENUM_VALUE "Location adjacent to a schema definition."
      .+ "SCALAR" .==
          'ENUM_VALUE "Location adjacent to a scalar definition."
      .+ "OBJECT" .==
          'ENUM_VALUE "Location adjacent to an object type definition."
      .+ "FIELD_DEFINITION" .==
          'ENUM_VALUE "Location adjacent to a field definition."
      .+ "ARGUMENT_DEFINITION" .==
          'ENUM_VALUE "Location adjacent to an argument definition."
      .+ "INTERFACE" .==
          'ENUM_VALUE "Location adjacent to an interface definition."
      .+ "UNION" .==
          'ENUM_VALUE "Location adjacent to a union definition."
      .+ "ENUM" .==
          'ENUM_VALUE "Location adjacent to an enum definition."
      .+ "ENUM_VALUE" .==
          'ENUM_VALUE "Location adjacent to an enum value definition."
      .+ "INPUT_OBJECT" .==
          'ENUM_VALUE "Location adjacent to an input object type definition."
      .+ "INPUT_FIELD_DEFINITION" .==
          'ENUM_VALUE "Location adjacent to an input object field definition."
      )


type TypeDefinition =
  "__Type" .==
    'OBJECT TypePlaceholder
      "The fundamental unit of any GraphQL Schema is the type. There are \
      \many kinds of types in GraphQL as represented by the `__TypeKind` enum.\
      \\n\nDepending on the kind of a type, certain fields describe \
      \information about that type. Scalar types provide no information \
      \beyond a name and description, while Enum types provide their values. \
      \Object and Interface types provide the fields they describe. Abstract \
      \types, Union and Interface, provide the Object types possible \
      \at runtime. List and NonNull types compose other types."
      '[]
      ( "kind" .==
          'FIELD Empty ('NON_NULL "__TypeKind") ""
      .+ "name" .==
          'FIELD Empty ('NULLABLE "String") ""
      .+ "description" .==
          'FIELD Empty ('NULLABLE "String") ""
      .+ "fields" .==
          'FIELD
            ( "includeDeprecated" .==
                'INPUT_VALUE_WITH_DEFAULT ('NULLABLE "Boolean") ""
            )
            ('NULLABLE_LIST_OF ('NON_NULL "__Field"))
            ""
      .+ "interfaces" .==
          'FIELD Empty ('NULLABLE_LIST_OF ('NON_NULL "__Type")) ""
      .+ "possibleTypes" .==
          'FIELD Empty ('NULLABLE_LIST_OF ('NON_NULL "__Type")) ""
      .+ "enumValues" .==
          'FIELD
            ( "includeDeprecated" .==
                'INPUT_VALUE_WITH_DEFAULT ('NULLABLE "Boolean") ""
            )
            ('NULLABLE_LIST_OF ('NON_NULL "__EnumValue"))
            ""
      .+ "inputFields" .==
           'FIELD
            ( "includeDeprecated" .==
                'INPUT_VALUE_WITH_DEFAULT ('NULLABLE "Boolean") ""
            )
            ('NULLABLE_LIST_OF ('NON_NULL "__InputValue"))
            ""
      .+ "ofType" .==
          'FIELD Empty ('NULLABLE "__Type") ""
      )


type TypeKindDefinition =
  "__TypeKind" .==
    'ENUM
      "An enum describing what kind of type a given `__Type` is."
      ( "SCALAR" .==
          'ENUM_VALUE "Indicates this type is a scalar."
      .+ "OBJECT" .==
          'ENUM_VALUE
            "Indicates this type is an object. \
            \`fields` and `interfaces` are valid fields."
      .+ "INTERFACE" .==
          'ENUM_VALUE
            "Indicates this tyle is an interface. \
            \`fields` and `possibleTypes` are valid fields."
      .+ "UNION" .==
          'ENUM_VALUE
            "Indicates this type is a union. \
            \`possibleTypes` is a valid field."
      .+ "ENUM" .==
          'ENUM_VALUE
            "Indicates this type is an enum. \
            \`enumValues` is a valid field."
      .+ "INPUT_OBJECT" .==
          'ENUM_VALUE
            "Indicates this type is an input object. \
            \`inputFields` is a valid field."
      .+ "LIST" .==
          'ENUM_VALUE
            "Indicates this type is a list. \
            \`ofType` is a valid field."
      .+ "NON_NULL" .==
          'ENUM_VALUE
            "Indicates this type is a non-null. \
            \`ofType` is a valid field."
      )


type FieldDef =
  "__Field" .==
    'OBJECT FieldPlaceholder
      "Object and Interface types are described by a list of Fields, each of \
      \which has a name, potentially a list of arguments, and a return type."
      '[]
      ( "name" .==
          'FIELD Empty ('NON_NULL "String") ""
      .+ "description" .==
          'FIELD Empty ('NULLABLE "String") ""
      .+ "args" .==
          'FIELD Empty ('NON_NULL_LIST_OF ('NON_NULL "__InputValue")) ""
      .+ "type" .==
          'FIELD Empty ('NON_NULL "__Type") ""
      .+ "isDeprecated" .==
          'FIELD Empty ('NON_NULL "Boolean") ""
      .+ "deprecationReason" .==
          'FIELD Empty ('NULLABLE "String") ""
      )


type InputValueDef =
  "__InputValue" .==
    'OBJECT InputValuePlaceholder
      "Arguments provided to Fields or dierctives and the input fields of an \
      \InputObject are represented as Input Values which describe their type \
      \and optionally a default value."
      '[]
      ( "name" .==
          'FIELD Empty ('NON_NULL "String") ""
      .+ "description" .==
          'FIELD Empty ('NULLABLE "String") ""
      .+ "type" .==
          'FIELD Empty ('NON_NULL "__Type") ""
      .+ "defaultValue" .==
          'FIELD Empty ('NULLABLE "String") ""
      )


type EnumValueDefinition =
  "__EnumValue" .==
    'OBJECT EnumValuePlaceholder
      "One possible value for a given Enum. Enum values are unique values, not \
      \a placeholder for a string or numeric value. However an Enum value is \
      \returned in a JSON response as a string."
      '[]
      ( "name" .==
          'FIELD Empty ('NON_NULL "String") ""
      .+ "description" .==
          'FIELD Empty ('NULLABLE "String") ""
      .+ "isDeprecated" .==
          'FIELD Empty ('NON_NULL "Boolean") ""
      .+ "deprecationReason" .==
          'FIELD Empty ('NULLABLE "String") ""
      )


type FieldOrInputValue =
  "FieldOrInputValue" .==
    'UNION
      "__Field or __InputValue"
      '[ "__Field"
       , "__InputValue"
       ]


type Schema = FieldDef .+ InputValueDef .+ FieldOrInputValue

type FieldOrInputValueT = GetType Schema ('NON_NULL "FieldOrInputValue")


unionValue :: forall label value row. (KnownSymbol label, row .! label ≈ value) => value -> Variant.Var row
unionValue = diversify . Variant.singleton Label
  where
    diversify :: Variant.Var (label .== value) -> Variant.Var row
    diversify = UNSAFE.unsafeCoerce

x :: InputValuePlaceholder
x = undefined


value :: Applicative m => m [Maybe FieldOrInputValueT]
value = pure . pure . pure $ unionValue @"__InputValue" x
