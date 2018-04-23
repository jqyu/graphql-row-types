module GraphQL.Type where

import           Data.Row  (Row)
import           GHC.Types (Symbol, Type)


data GraphQLType where
  Nullable :: Symbol -> GraphQLType
  NullableListOf :: GraphQLType -> GraphQLType
  NonNull :: Symbol -> GraphQLType
  NonNullListOf :: GraphQLType -> GraphQLType

type Name = Symbol
type Description = Symbol
type DeprecationReason = Symbol
type Interface = Symbol

data GraphQLTypeDefinition where
  Scalar
    :: Type
    -> Description
    -> GraphQLTypeDefinition
  Object
    :: Type
    -> Description
    -> [Interface]
    -> Row FieldDefinition
    -> GraphQLTypeDefinition
  Interface
    ::  Description
    -> Row FieldDefinition
    -> GraphQLTypeDefinition
  Union
    :: Description
    -> [Name]
    -> GraphQLTypeDefinition
  Enum
    :: Description
    -> Row EnumValueDefinition
    -> GraphQLTypeDefinition
  InputObject
    :: Description
    -> Row InputValueDefinition
    -> GraphQLTypeDefinition


data FieldDefinition where
  Field
    :: Row ArgumentDefinition
    -> GraphQLType
    -> Description
    -> FieldDefinition
  DeprecatedField
    :: Row ArgumentDefinition
    -> GraphQLType
    -> DeprecationReason
    -> Description
    -> FieldDefinition


type ArgumentDefinition = InputValueDefinition

data InputValueDefinition where
  InputValue
    :: GraphQLType
    -> Description
    -> InputValueDefinition
  InputValueWithDefault
    :: GraphQLType
    -> Description
    -> InputValueDefinition

data EnumValueDefinition where
  EnumValue
    :: Description
    -> EnumValueDefinition
  DeprecatedEnumValue
    :: Description
    -> DeprecationReason
    -> EnumValueDefinition
