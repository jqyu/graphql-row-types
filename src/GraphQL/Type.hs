{-# LANGUAGE UndecidableInstances #-}

module GraphQL.Type where

import Data.Row (Rec, Var, type (.+), type (.==))
import Data.Row.Poly (type (.!), Empty, Ifte, LT(..), Row(..))
import GHC.Types (Symbol, Type)
import Protolude

-- schema representation
data TypeRef where
  NULLABLE :: Symbol -> TypeRef
  NULLABLE_LIST_OF :: TypeRef -> TypeRef
  NON_NULL :: Symbol -> TypeRef
  NON_NULL_LIST_OF :: TypeRef -> TypeRef

type Name = Symbol

type Description = Symbol

type DeprecationReason = Symbol

type Interface = Symbol

data TypeDefinition where
  SCALAR
    :: Type
    -> Description
    -> TypeDefinition
  OBJECT
    :: Type
    -> Description
    -> [Interface]
    -> Row FieldDefinition
    -> TypeDefinition
  INTERFACE
    :: Description
    -> Row FieldDefinition
    -> TypeDefinition
  UNION
    :: Description
    -> [Name]
    -> TypeDefinition
  ENUM
    :: Description
    -> Row EnumValueDefinition
    -> TypeDefinition
  INPUT_OBJECT
    :: Description
    -> Row InputValueDefinition
    -> TypeDefinition

data FieldDefinition where
  FIELD
    :: Row ArgumentDefinition
    -> TypeRef
    -> Description
    -> FieldDefinition
  DEPRECATED_FIELD
    :: Row ArgumentDefinition
    -> TypeRef
    -> DeprecationReason
    -> Description
    -> FieldDefinition

type ArgumentDefinition = InputValueDefinition

data InputValueDefinition where
  INPUT_VALUE
    :: TypeRef
    -> Description
    -> InputValueDefinition
  INPUT_VALUE_WITH_DEFAULT
    :: TypeRef
    -> Symbol
    -> Description
    -> InputValueDefinition

data EnumValueDefinition where
  ENUM_VALUE
    :: Description
    -> EnumValueDefinition
  DEPRECATED_ENUM_VALUE
    :: DeprecationReason
    -> Description
    -> EnumValueDefinition

-- type families to determine corresponding types

type family GetType (schema :: Row TypeDefinition) (ref :: TypeRef) :: Type where
  GetType schema ('NON_NULL symbol) = GetNamedType schema symbol
  GetType schema ('NON_NULL_LIST_OF ref) = [GetType schema ref]
  GetType schema ('NULLABLE symbol) = Maybe (GetNamedType schema symbol)
  GetType schema ('NULLABLE_LIST_OF ref) = Maybe [GetType schema ref]

type family GetNamedType (schema :: Row TypeDefinition) (name :: Symbol) :: Type where
  GetNamedType schema name = DefinitionToType schema name (schema .! name)

type family DefinitionToType (schema :: Row TypeDefinition) (name :: Symbol) (definition :: TypeDefinition) :: Type where
  DefinitionToType _ _ ('SCALAR scalar _) = scalar
  DefinitionToType _ _ ('OBJECT object _ _ _) = object
  DefinitionToType schema interface ('INTERFACE _ _) = Var (InterfaceToRow schema interface)
  DefinitionToType schema _ ('UNION _ possibleTypes) = Var (UnionToRow schema possibleTypes)
  DefinitionToType schema _ ('INPUT_OBJECT _ inputFields) = Rec (InputValuesToRow schema inputFields)

type family InterfaceToRow (schema :: Row TypeDefinition) (interface :: Symbol) :: Row Type where
  InterfaceToRow ('R definitions) interface = 'R (InterfaceToRowR interface definitions)

type family InterfaceToRowR (interface :: Symbol) (definitions :: [LT TypeDefinition]) :: [LT Type] where
  InterfaceToRowR _ '[] = '[]
  InterfaceToRowR interface (label :-> 'OBJECT object _ interfaces _ ': rest) =
    Ifte
      (Elem interface interfaces)
      (label :-> object ': InterfaceToRowR interface rest)
      (InterfaceToRowR interface rest)
  InterfaceToRowR interface (_ ': rest) = InterfaceToRowR interface rest

type family UnionToRow (schema :: Row TypeDefinition) (possibleTypes :: [Symbol]) :: Row Type where
  UnionToRow schema '[] = Empty
  UnionToRow schema (label ': rest) =
    label .== DefinitionToType schema label (schema .! label)
    .+ UnionToRow schema rest

type family InputValuesToRow (schema :: Row TypeDefinition) (inputValues :: Row InputValueDefinition) :: Row Type where
  InputValuesToRow schema ('R inputValues) = 'R (InputValuesToRowR schema inputValues)

type family InputValuesToRowR (schema :: Row TypeDefinition) (inputValues :: [LT InputValueDefinition]) :: [LT Type] where
  InputValuesToRowR _ '[] = '[]
  InputValuesToRowR schema (label :-> 'INPUT_VALUE ref _ ': rest) =
    label :-> GetType schema ref
    ': InputValuesToRowR schema rest
  InputValuesToRowR schema (label :-> 'INPUT_VALUE_WITH_DEFAULT ref _ _ ': rest) =
    label :-> GetNullableInputValueType schema ref
    ': InputValuesToRowR schema rest

type family GetNullableInputValueType (schema :: Row TypeDefinition) (ref :: TypeRef) :: Type where
  GetNullableInputValueType schema ('NULLABLE symbol) = GetNamedType schema symbol
  GetNullableInputValueType schema ('NULLABLE_LIST_OF ref) = Maybe [GetType schema ref]
  -- TODO: error message in case you try to provide a non-null input value with a default value


newtype GraphQLEnum (cases :: [Symbol]) = GraphQLEnum { value :: Text }

-- functions on type level lists

type family Elem (value :: k) (values :: [k]) :: Bool where
  Elem value '[] = False
  Elem value (value ': _) = True
  Elem value (_ ': rest) = Elem value rest
