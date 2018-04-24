
{-# LANGUAGE UndecidableInstances #-}

module GraphQL.Type where

import Protolude
import Data.Row (type (.==), type (.+))
import Data.Row.Variants (Var)
import Data.Row.Poly (LT(..), Row(..), Empty, Ifte, type (.!))
import GHC.Types (Symbol, Type)


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
    -> Description
    -> InputValueDefinition


data EnumValueDefinition where
  ENUM_VALUE
    :: Description
    -> EnumValueDefinition
  DEPRECATED_ENUM_VALUE
    :: Description
    -> DeprecationReason
    -> EnumValueDefinition


-- type families to determine corresponding types

type family GetType (schema :: Row TypeDefinition) (ref :: TypeRef) :: Type where
  GetType schema ('NON_NULL symbol) = GetNamedType schema symbol
  GetType schema ('NON_NULL_LIST_OF ref) = [GetType schema ref]
  GetType schema ('NULLABLE symbol) = GetNamedType schema symbol
  GetType schema ('NULLABLE_LIST_OF ref) = Maybe [GetType schema ref]


type family GetNamedType (schema :: Row TypeDefinition) (name :: Symbol) :: Type where
  GetNamedType schema name = DefinitionToType schema name (schema .! name)


type family DefinitionToType (schema :: Row TypeDefinition) (name :: Symbol) (definition :: TypeDefinition) :: Type where
  DefinitionToType _ _ ('SCALAR scalar _) = scalar
  DefinitionToType _ _ ('OBJECT object _ _ _) = object
  DefinitionToType schema interface ('INTERFACE _ _) = Var (InterfaceToRow interface schema)
  DefinitionToType schema _ ('UNION _ possibleTypes) = Var (UnionToRow possibleTypes schema)

type family InterfaceToRow (interface :: Symbol) (schema :: Row TypeDefinition) :: Row Type where
  InterfaceToRow interface ('R definitions) = 'R (InterfaceToRowR interface definitions)

type family InterfaceToRowR (interface :: Symbol) (schema :: [LT TypeDefinition]) :: [LT Type] where
  InterfaceToRowR _ '[] = '[]
  InterfaceToRowR interface (label :-> 'OBJECT object _ interfaces _ ': rest) =
    Ifte (Elem interface interfaces)
    (label :-> object ': InterfaceToRowR interface rest)
    (InterfaceToRowR interface rest)
  InterfaceToRowR interface (_ ': rest) = InterfaceToRowR interface rest

type family UnionToRow (possibleTypes :: [Symbol]) (schema :: Row TypeDefinition) :: Row Type where
  UnionToRow '[] schema = Empty
  UnionToRow (label ': rest) schema =
    (label .== DefinitionToType schema label (schema .! label))
    .+ UnionToRow rest schema

type family Elem (value :: k) (values :: [k]) :: Bool where
  Elem value '[] = False
  Elem value (v ': rest) = Ifte (value == v) True (Elem value rest)
