module GraphQLExample where

import Protolude

import Data.Row.Poly (Empty, type (.==), type (.+))
import GraphQL.Type


newtype Id = Id { unId :: Text } deriving (Eq, Ord, Show)


type IntDefinition =
  "Int" .==
    'Scalar Int32
      "The `Int` scalar type represents non-fractional signed whole numeric \
      \values. Int can represent values between -(2^31) and 2^31 - 1."

type FloatDefinition =
  "Float" .==
    'Scalar Double
      "The `Float` scalar type represents signed double-precision fractional \
      \values as specified by \
      \[IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."

type StringDefinition =
  "String" .==
    'Scalar Text
      "The `String` scalar type represents textual data, represented as UTF-8 \
      \character sequences. The String type is most often used by GraphQL to \
      \represent free-form human-readable text."

type BooleanDefinition =
  "Boolean" .==
    'Scalar Bool
      "The `Boolean` scalar type represents `true` or `false`."

type IdDefinition =
  "ID" .==
    'Scalar Id
      "The `ID` scalar type represents a unique identifier, often used to \
      \refetch an object or as key for a cache. The ID type appears in a JSON \
      \response as a String; however, it is not intended to be human-readable. \
      \When expected as an input type, any string (such as `\"4\"`) or integer \
      \(such as `4`) input value will be accepted as an ID."

data SchemaPlaceholder
data TypePlaceholder
data FieldPlaceholder
data InputValuePlaceholder
data EnumValuePlaceholder
data DirectivePlaceholder

type SchemaDefinition =
  "__Schema" .==
    'Object SchemaPlaceholder
      "A GraphQL Schema defines the capabilities of a GraphQL server. It \
      \exposes all available types and directives on the server, as well as \
      \the entry points for query, mutation, and subscription operations."
      '[]
      ( "types" .==
          'Field Empty ('NonNullListOf ('NonNull "__Type"))
            "A list of all types supported by this server."
      .+ "queryType" .==
          'Field Empty ('NonNull "__Type")
            "The type that query operations will be rooted at."
      .+ "mutationType" .==
          'Field Empty ('Nullable "__Type")
            "If this server supports mutation, the type that \
            \mutation operations will be rooted at."
      .+ "subscriptionType" .==
          'Field Empty ('Nullable "__Type")
            "If this server supports subscription, the type that \
            \mutation operations will be rooted at."
      .+ "directives" .==
          'Field Empty ('NonNullListOf ('NonNull "__Type"))
            "A list of all directives supported by this server."
      )

type DirectiveDefinition =
  "__Directive" .==
    'Object DirectivePlaceholder
      "A Directive provides a way to describe alternate runtime execution and \
      \type validation behavior in a GraphQL document.\
      \\n\nIn some cases, you need to provide options to alter GraphQL's \
      \execution behavior in ways field arguments will not suffice, such as \
      \conditionally including or skipping a field. Directives provide this by \
      \describing additional information to the executor."
      '[]
      ( "name" .==
          'Field Empty ('NonNull "String") ""
      .+ "description" .==
          'Field Empty ('Nullable "String") ""
      .+ "locations" .==
          'Field Empty ('NonNullListOf ('NonNull "__DirectiveLocation")) ""
      .+ "args" .==
          'Field Empty ('NonNullListOf ('NonNull "__InputValue")) ""
      -- NOTE: the following three fields are deprecated and are no longer part
      -- of the GraphQL specification.
      .+ "onOperation" .==
          'DeprecatedField Empty ('NonNull "Boolean") ""
            "Use `locations`."
      .+ "onFragment" .==
          'DeprecatedField Empty ('NonNull "Boolean") ""
            "Use `locations`."
      .+ "onField" .==
          'DeprecatedField Empty ('NonNull "Boolean") ""
            "Use `locations`."
      )

type DirectiveLocationDefinition =
  "__DirectiveLocation" .==
    'Enum
      "A Directive can be adjacent to many parts of the GraphQL language, a \
      \__DirectiveLocation describes one such possible adjacencies."
      ( "QUERY" .==
          'EnumValue "Location adjacent to a query operation."
      .+ "MUTATION" .==
          'EnumValue "Location adjacent to a mutation operation."
      .+ "SUBSCRIPTION" .==
          'EnumValue "Location adjacent to a subscription operation."
      .+ "FIELD" .==
          'EnumValue "Location adjacent to a field."
      .+ "FRAGMENT_DEFINITION" .==
          'EnumValue "Location adjacent to a fragment definition."
      .+ "FRAGMENT_SPREAD" .==
          'EnumValue "Location adjacent to a fragment spread."
      .+ "INLINE_FRAGMENT" .==
          'EnumValue "Location adjacent to an inline fragment."
      .+ "SCHEMA" .==
          'EnumValue "Location adjacent to a schema definition."
      .+ "SCALAR" .==
          'EnumValue "Location adjacent to a scalar definition."
      .+ "OBJECT" .==
          'EnumValue "Location adjacent to an object type definition."
      .+ "FIELD_DEFINITION" .==
          'EnumValue "Location adjacent to a field definition."
      .+ "ARGUMENT_DEFINITION" .==
          'EnumValue "Location adjacent to an argument definition."
      .+ "INTERFACE" .==
          'EnumValue "Location adjacent to an interface definition."
      .+ "UNION" .==
          'EnumValue "Location adjacent to a union definition."
      .+ "ENUM" .==
          'EnumValue "Location adjacent to an enum definition."
      .+ "ENUM_VALUE" .==
          'EnumValue "Location adjacent to an enum value definition."
      .+ "INPUT_OBJECT" .==
          'EnumValue "Location adjacent to an input object type definition."
      .+ "INPUT_FIELD_DEFINITION" .==
          'EnumValue "Location adjacent to an input object field definition."
      )


type TypeDefinition =
  "__Type" .==
    'Object TypePlaceholder
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
          'Field Empty ('NonNull "__TypeKind") ""
      .+ "name" .==
          'Field Empty ('Nullable "String") ""
      .+ "description" .==
          'Field Empty ('Nullable "String") ""
      .+ "fields" .==
          'Field
            ( "includeDeprecated" .==
                'InputValueWithDefault ('Nullable "Boolean") ""
            )
            ('NullableListOf ('NonNull "__Field"))
            ""
      .+ "interfaces" .==
          'Field Empty ('NullableListOf ('NonNull "__Type")) ""
      .+ "possibleTypes" .==
          'Field Empty ('NullableListOf ('NonNull "__Type")) ""
      .+ "enumValues" .==
          'Field
            ( "includeDeprecated" .==
                'InputValueWithDefault ('Nullable "Boolean") ""
            )
            ('NullableListOf ('NonNull "__EnumValue"))
            ""
      .+ "inputFields" .==
           'Field
            ( "includeDeprecated" .==
                'InputValueWithDefault ('Nullable "Boolean") ""
            )
            ('NullableListOf ('NonNull "__InputValue"))
            ""
      .+ "ofType" .==
          'Field Empty ('Nullable "__Type") ""
      )


type TypeKindDefinition =
  "__TypeKind" .==
    'Enum
      "An enum describing what kind of type a given `__Type` is."
      ( "SCALAR" .==
          'EnumValue "Indicates this type is a scalar."
      .+ "OBJECT" .==
          'EnumValue
            "Indicates this type is an object. \
            \`fields` and `interfaces` are valid fields."
      .+ "INTERFACE" .==
          'EnumValue
            "Indicates this tyle is an interface. \
            \`fields` and `possibleTypes` are valid fields."
      .+ "UNION" .==
          'EnumValue
            "Indicates this type is a union. \
            \`possibleTypes` is a valid field."
      .+ "ENUM" .==
          'EnumValue
            "Indicates this type is an enum. \
            \`enumValues` is a valid field."
      .+ "INPUT_OBJECT" .==
          'EnumValue
            "Indicates this type is an input object. \
            \`inputFields` is a valid field."
      .+ "LIST" .==
          'EnumValue
            "Indicates this type is a list. \
            \`ofType` is a valid field."
      .+ "NON_NULL" .==
          'EnumValue
            "Indicates this type is a non-null. \
            \`ofType` is a valid field."
      )


type FieldDefinition =
  "__Field" .==
    'Object FieldPlaceholder
      "Object and Interface types are described by a list of Fields, each of \
      \which has a name, potentially a list of arguments, and a return type."
      '[]
      ( "name" .==
          'Field Empty ('NonNull "String") ""
      .+ "description" .==
          'Field Empty ('Nullable "String") ""
      .+ "args" .==
          'Field Empty ('NonNullListOf ('NonNull "__InputValue")) ""
      .+ "type" .==
          'Field Empty ('NonNull "__Type") ""
      .+ "isDeprecated" .==
          'Field Empty ('NonNull "Boolean") ""
      .+ "deprecationReason" .==
          'Field Empty ('Nullable "String") ""
      )


type InputValueDefinition =
  "__InputValue" .==
    'Object InputValuePlaceholder
      "Arguments provided to Fields or dierctives and the input fields of an \
      \InputObject are represented as Input Values which describe their type \
      \and optionally a default value."
      '[]
      ( "name" .==
          'Field Empty ('NonNull "String") ""
      .+ "description" .==
          'Field Empty ('Nullable "String") ""
      .+ "type" .==
          'Field Empty ('NonNull "__Type") ""
      .+ "defaultValue" .==
          'Field Empty ('Nullable "String") ""
      )


type EnumValueDefinition =
  "__EnumValue" .==
    'Object EnumValuePlaceholder
      "One possible value for a given Enum. Enum values are unique values, not \
      \a placeholder for a string or numeric value. However an Enum value is \
      \returned in a JSON response as a string."
      '[]
      ( "name" .==
          'Field Empty ('NonNull "String") ""
      .+ "description" .==
          'Field Empty ('Nullable "String") ""
      .+ "isDeprecated" .==
          'Field Empty ('NonNull "Boolean") ""
      .+ "deprecationReason" .==
          'Field Empty ('Nullable "String") ""
      )
